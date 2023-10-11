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
source('metrics/thresholds.R')
source('metrics/exposure_response_functions.R')

input_path = paste0(here::here(), '/analysis/population_noise_exposure/_output/noise_exposure')
output_path = paste0(here::here(), '/analysis/population_health_impacts/_output')
pop_exposure_stack = stack(glue('{input_path}/pop_exposure_stack.grd'))
pop_areas_stack = stack(glue('{input_path}/pop_areas_stack.grd'))

msg('Starting health impact assessment...')
filename_output = glue(output_path, '/health_impact_output.txt')
sink(filename_output, split=T)

## Number of people exposed to levels beyond bounds of common ERFs
# Annoyance (75, ISO, WHO, FAA NES)
msg('Number of people exposed to levels beyond bounds of common ERFs:')
HA_exceed = round(cellStats(mask(pop_exposure_stack[['Exposed.Population']], clamp(pop_exposure_stack[['Ldn']], lower=bounds_who[2], useValues=F)), 'sum'))
msg(' HA ', HA_exceed)
# Sleep disturbance (65, WHO/Smith)
HSD_exceed = round(cellStats(mask(pop_exposure_stack[['Exposed.Population']], clamp(pop_exposure_stack[['Lnight']], lower=bounds_HSD[2], useValues=F)), 'sum'))
msg(' HSD ', HSD_exceed)

# Calculate health metrics for each zone
health_impact_summary = data.frame()
health_impact_layers = list()
for (zone in names(pop_areas_stack)) {
  zone_name = unlist(str_split(zone, '_'))[1]
  type = unlist(str_split(zone, '_'))[2]
  msg(zone_name, ' (', type, ')', sep='')
  
  # Total exposed population
  zone_pop = pop_areas_stack[[zone]]
  npop_zone = cellStats(zone_pop, 'sum')
  dasy_pop_cropped = crop(zone_pop, pop_exposure_stack[['Ldn']])
  dasy_pop_masked = mask(dasy_pop_cropped, pop_exposure_stack[['Ldn']])
  dasy_pop_masked[dasy_pop_masked == 0] = NA # set all 0 population cells to NA
  estimated_pop_exposed = dasy_pop_masked
  
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
  
  # Add results to table
  health_impact_summary = rbind(health_impact_summary, data.frame(
    Type         = type,
    Name         = zone_name,
    Population   = npop_zone,
    HA_FICON     = npop_HA_FICON,
    HA_ISO       = npop_HA_ISO,
    HA_WHO       = npop_HA_WHO,
    HA_FAANES    = npop_HA_FAANES,
    HA_Yokoshima = npop_HA_Yokoshima,
    HSD_WHO      = npop_HSD_WHO,
    HSD_Smith    = npop_HSD_Smith
  ))
}

# Format table and calculate totals for the entire study region
health_impact_summary = health_impact_summary %>% mutate_at(c(3:ncol(health_impact_summary)), round)
health_impact_summary = health_impact_summary[rowSums(health_impact_summary[4:ncol(health_impact_summary)])>0,] # remove counties with no exposure
health_impact_summary = health_impact_summary[order(health_impact_summary$HA_WHO, decreasing=T), ]

totals = health_impact_summary[health_impact_summary$Type == 'county', ] %>% summarise(.,
                                              across(where(is.numeric), sum),
                                              across(where(is.character), ~"Total"))

# Append percentages
backup = health_impact_summary
for (r in 1:nrow(health_impact_summary)) {
  health_impact_summary[r, 4:ncol(health_impact_summary)] = paste0(
    as.character(health_impact_summary[r, 4:ncol(health_impact_summary)]),
    ' (', round((as.numeric(health_impact_summary[r, 4:ncol(health_impact_summary)]) / as.numeric(health_impact_summary[r, 3])) * 100,1), ')')
}

health_impact_summary = rbind(health_impact_summary, totals)
health_impact_summary = format(health_impact_summary, big.mark = ',', trim=T)
health_impact_summary = health_impact_summary[,2:ncol(health_impact_summary)]

msg(health_impact_summary)

# Write table and results to files
sink()
msg('Created', filename_output)
filename = glue(output_path, '/health_impact_summary.csv')
write.csv(health_impact_summary, filename, row.names = F)
msg('Created', filename)
