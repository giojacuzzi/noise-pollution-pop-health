library(raster)
library(sf)
library(glue)
library(gdalUtils)
library(rslurm)
library(fasterize)
library(mapview)
mapviewOptions(mapview.maxpixels = 50000000)
options(tigris_use_cache = T)

source('global.R')
source('simulation/contours.R')

source('metrics/metrics.R')
source('metrics/health_metrics.R')
source('metrics/exposure_response_functions.R')

input_path = paste0(here::here(), '/analysis/preprocessing/_output')
output_path = paste0(here::here(), '/analysis/_output')
pop_exposure_stack = stack(glue('{input_path}/pop_exposure_stack.grd'))
pop_zones_stack = stack(glue('{input_path}/pop_zones_stack.grd'))

## Spatial distribution of noise ---------------------------------------------------------------------
# Note that these numbers may be conservative estimates due to the lack of evening-time penalty in DNL calculations, and depending on the exposure-response function used. These numbers also implicitly assume long-term (in some cases yearly average) exposure.

msg('Starting health assessment summary...')
filename_output = glue(output_path, '/health_impact_output.txt')
sink(filename_output, split=T)

# mapview(pop_exposure_stack[['Exposed.Population']], layer.name=c('Exposed Persons')) +
#   mapview(pop_exposure_stack[['Ldn']], layer.name=c('Ldn (dB)')) +
#   mapview(pop_exposure_stack[['Lnight']], layer.name=c('Lnight (dB)')) +
#   mapview(pop_exposure_stack[['Leq24']], layer.name=c('Leq24 (dB)'))

msg('Total exposed population:', round(cellStats(pop_exposure_stack[['Exposed.Population']], 'sum')))

# Total area of noise exposure associated with adverse health effects (note this includes water)
contours_Ldn = get_contours_Ldn()
impact_area = st_area(st_make_valid(st_union(contours_Ldn[contours_Ldn$Level>=lden_impact_threshold,])))
msg('Total area of noise exposure associated with adverse health effects:', round(units::set_units(impact_area, km^2),2), 'km2', round(units::set_units(impact_area, mi^2),2), 'mi2')

## Number of people exposed to noise levels incompatible with residential land use (65 dB Ldn, FAA and HUD)
residential_exposed = round(cellStats(mask(pop_exposure_stack[['Exposed.Population']], clamp(pop_exposure_stack[['Ldn']], lower=residential_impact_threshold, useValues=F)), 'sum'))
msg('Number of people exposed to noise levels incompatible with residential land use (65 dB Ldn, FAA and HUD):', residential_exposed)

## Health impacts ---------------------------------------------------------------------

## Number of people exposed to levels beyond bounds of common ERFs
# Annoyance (75, ISO, WHO, FAA NES)
msg('Number of people exposed to levels beyond bounds of common ERFs:')
HA_exceed = round(cellStats(mask(pop_exposure_stack[['Exposed.Population']], clamp(pop_exposure_stack[['Ldn']], lower=bounds_who[2], useValues=F)), 'sum'))
msg(' HA ', HA_exceed)
# Sleep disturbance (65, WHO/Smith)
HSD_exceed = round(cellStats(mask(pop_exposure_stack[['Exposed.Population']], clamp(pop_exposure_stack[['Lnight']], lower=bounds_HSD[2], useValues=F)), 'sum'))
msg(' HSD ', HSD_exceed)

## Number of people exposed to sleeping disturbance risk threshold
HSD_exposed = round(cellStats(mask(pop_exposure_stack[['Exposed.Population']], clamp(pop_exposure_stack[['Lnight']], lower=lnight_impact_threshold, useValues=F)), 'sum'))
msg('Number of people exposed to sleeping disturbance risk threshold:', HSD_exposed)

# Precalculate rasters for HA, HSD, HL/CE
msg('Precalculating raster for HA (FICON)...')
percent_HA_FICON = calc(pop_exposure_stack[['Ldn']],
                        fun=function(L) { exp_resp_bounded(exp_resp_FICON, L, bounds_FICON) })
msg('Precalculating raster for HA (ISO)...')
percent_HA_ISO = calc(pop_exposure_stack[['Ldn']],
                      fun=function(L) { exp_resp_bounded(exp_resp_ISO_Miedema_Ldn, L, bounds_iso_miedema) })
msg('Precalculating raster for HA (WHO)...')
percent_HA_WHO = calc(pop_exposure_stack[['Ldn']],
                      fun=function(L) { exp_resp_bounded(exp_resp_WHO, L, bounds_who) })
msg('Precalculating raster for HA (FAA NES)...')
percent_HA_FAANES = calc(pop_exposure_stack[['Ldn']],
                         fun=function(L) { exp_resp_bounded(exp_resp_FAANES, L, bounds_FAANES) })
msg('Precalculating raster for HA (Yokoshima)...')
percent_HA_Yokoshima = calc(pop_exposure_stack[['Ldn']],
                            fun=function(L) { exp_resp_bounded(exp_resp_Yokoshima, L, bounds_Yokoshima) })
msg('Precalculating raster for HSD (WHO)...')
percent_HSD_WHO = calc(pop_exposure_stack[['Lnight']],
                       fun=function(L) { exp_resp_bounded(exp_resp_HSD_WHO, L, bounds_HSD) })
msg('Precalculating raster for HSD (Smith)...')
percent_HSD_Smith = calc(pop_exposure_stack[['Lnight']],
                         fun=function(L) { exp_resp_bounded(exp_resp_HSD_Smith, L, bounds_HSD) })
msg('Precalculating raster for HL/CE...')
area_Leq24_HLCE = clamp(pop_exposure_stack[['Leq24']], lower=HL_leq24_impact_threshold, useValues=F) # 70 dB Leq24 and up

# Calculate health metrics for each zone
health_impact_summary = data.frame()
health_impact_layers = list()
for (zone in names(pop_zones_stack)) {
  zone_name = unlist(str_split(zone, '_'))[1]
  type = unlist(str_split(zone, '_'))[2]
  msg(zone_name, ' (', type, ')', sep='')
  
  # Total zone population
  zone_pop = pop_zones_stack[[zone]]
  npop_zone = cellStats(zone_pop, 'sum')
  msg('  Total        ', npop_zone)
  
  # Adverse health exposure
  dasy_pop_cropped = crop(zone_pop, pop_exposure_stack[['Ldn']])
  dasy_pop_masked = mask(dasy_pop_cropped, pop_exposure_stack[['Ldn']])
  dasy_pop_masked[dasy_pop_masked == 0] = NA # set all 0 population cells to NA
  estimated_pop_exposed = dasy_pop_masked
  npop_exposed = cellStats(estimated_pop_exposed, 'sum')
  msg('  Exposed      ', npop_exposed)
  
  ## Annoyance
  # Multiply population in each cell by %HA to yield estimate of # highly annoyed persons in that cell
  estimated_pop_HA_FICON = percent_HA_FICON * 0.01 * estimated_pop_exposed
  npop_HA_FICON = cellStats(estimated_pop_HA_FICON, 'sum')
  msg('  HA FICON ', npop_HA_FICON)
  
  estimated_pop_HA_ISO = percent_HA_ISO * 0.01 * estimated_pop_exposed
  npop_HA_ISO = cellStats(estimated_pop_HA_ISO, 'sum')
  msg('  HA ISO       ', npop_HA_ISO)

  estimated_pop_HA_WHO = percent_HA_WHO * 0.01 * estimated_pop_exposed
  npop_HA_WHO = cellStats(estimated_pop_HA_WHO, 'sum')
  msg('  HA WHO       ', npop_HA_WHO)
  
  estimated_pop_HA_FAANES = percent_HA_FAANES * 0.01 * estimated_pop_exposed
  npop_HA_FAANES = cellStats(estimated_pop_HA_FAANES, 'sum')
  msg('  HA FAANES ', npop_HA_FAANES)
  
  estimated_pop_HA_Yokoshima = percent_HA_Yokoshima * 0.01 * estimated_pop_exposed
  npop_HA_Yokoshima = cellStats(estimated_pop_HA_Yokoshima, 'sum')
  msg('  HA Yokoshima ', npop_HA_Yokoshima)
  
  ## Sleep disturbance
  # Multiply population in each cell by %HSD to yield estimate of # sleep-disturbed persons in that cell
  estimated_pop_HSD_WHO = percent_HSD_WHO * 0.01 * estimated_pop_exposed
  npop_HSD_WHO = cellStats(estimated_pop_HSD_WHO, 'sum')
  msg('  HSD WHO        ', npop_HSD_WHO)
  
  estimated_pop_HSD_Smith = percent_HSD_Smith * 0.01 * estimated_pop_exposed
  npop_HSD_Smith = cellStats(estimated_pop_HSD_Smith, 'sum')
  msg('  HSD Smith      ', npop_HSD_Smith)
  
  ## Hearing impairment and cardiovascular effects
  # Sum population cells exposed to levels causing hearing impairment and cardiovascular effects over time
  estimated_pop_HLCE = mask(estimated_pop_exposed, area_Leq24_HLCE)
  npop_HLCE = cellStats(estimated_pop_HLCE, 'sum')
  msg('  HLCE          ', npop_HLCE)
  
  # Add results to table
  health_impact_summary = rbind(health_impact_summary, data.frame(
    Type         = type,
    Name         = zone_name,
    Population   = npop_zone,
    Exposed      = npop_exposed,
    HA_FICON     = npop_HA_FICON,
    HA_ISO       = npop_HA_ISO,
    HA_WHO       = npop_HA_WHO,
    HA_FAANES    = npop_HA_FAANES,
    HA_Yokoshima = npop_HA_Yokoshima,
    HSD_WHO      = npop_HSD_WHO,
    HSD_Smith    = npop_HSD_Smith,
    HLCE         = npop_HLCE
  ))
}

# Subset only the counties and check values
health_impact_summary_counties = health_impact_summary[health_impact_summary$Type == 'county', ]
stopifnot(sum(health_impact_summary_counties$Exposed) == cellStats(pop_exposure_stack[['Exposed.Population']], 'sum'))

# Percent exposed per zone
msg('Percent exposed per zone:')
data.frame(
  health_impact_summary$Name,
  PercentExposed = health_impact_summary$Exposed / health_impact_summary$Population
)

# Format table and calculate totals for the entire study region
health_impact_summary = health_impact_summary %>% mutate_at(c(3:ncol(health_impact_summary)), round)
health_impact_summary = health_impact_summary[health_impact_summary$Exposed != 0, ] # remove counties with no exposure
health_impact_summary = health_impact_summary[order(health_impact_summary$Exposed, decreasing=T), ]

totals = health_impact_summary[health_impact_summary$Type == 'county', ] %>% summarise(.,
                                              across(where(is.numeric), sum),
                                              across(where(is.character), ~"Total"))

# Append percentages
health_impact_summary$Exposed = paste0(health_impact_summary$Exposed, ' (', round(health_impact_summary$Exposed / health_impact_summary$Population, 3) * 100, '%)')

health_impact_summary = rbind(health_impact_summary, totals)

# Manually enter zone names
health_impact_summary = health_impact_summary[,2:ncol(health_impact_summary)]
health_impact_summary$Name = c(
  'Island County',
  'Skagit County',
  'Samish TDSA',
  'Swinomish Reservation',
  'Jefferson County',
  'San Juan County',
  'Total*'
)

msg(health_impact_summary)

# Write table and results to files
sink()
msg('Created', filename_output)
filename = glue(output_path, '/health_impact_summary.csv')
write.csv(health_impact_summary, filename, row.names = F)
msg('Created', filename)
