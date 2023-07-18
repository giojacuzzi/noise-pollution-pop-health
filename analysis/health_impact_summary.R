library(tidycensus)
library(raster)
library(sf)
library(glue)
library(gdalUtils)
library(rslurm)
library(fasterize)
library(mapview)
mapviewOptions(mapview.maxpixels = 50000000)
options(tigris_use_cache = T)
census_api_key('a9d9f05e0560c7fadaed6b4168bedc56d0e4686d')

source('analysis/spatial_distribution.R')

source('metrics/metrics.R')
source('metrics/health_metrics.R')
source('metrics/exposure_response_functions.R')

input_path = paste0(here::here(), '/analysis/_output')
output_path = paste0(here::here(), '/analysis/_output')
pop_exposure_stack = stack(glue('{input_path}/pop_exposure_stack.grd'))

mapview(get_flighttracks()) +
  mapview(pop_exposure_stack[['Ldn']], layer.name=c('Ldn (dB)')) +
  mapview(pop_exposure_stack[['Impacted.Population']], layer.name=c('Impacted Persons')) +
  mapview(pop_exposure_stack[['Lnight']], layer.name=c('Lnight (dB)')) +
  mapview(pop_exposure_stack[['Leq24']], layer.name=c('Leq24 (dB)'))

# Precalculate rasters for HA, HSD, HL
message('Precalculating raster for HA (ISO)...')
percent_HA_ISO = calc(pop_exposure_stack[['Ldn']], fun=exp_resp_ISO_Miedema_Ldn_bounded)
message('Precalculating raster for HA (WHO)...')
percent_HA_WHO = calc(pop_exposure_stack[['Ldn']], fun=exp_resp_WHO_bounded)
message('Precalculating raster for HA (Yokoshima)...')
percent_HA_Yokoshima = calc(pop_exposure_stack[['Ldn']], fun=exp_resp_Yokoshima_bounded)
message('Precalculating raster for HSD...')
percent_HSD = calc(pop_exposure_stack[['Lnight']], fun=exp_resp_HSD_combinedestimate_bounded)
message('Precalculating raster for HL...')
area_Leq24_HL = clamp(pop_exposure_stack[['Leq24']], lower=HL_leq24_impact_threshold, useValues=F) # 70 dB Leq24 and up

# Calculate health metrics for each county
health_impact_summary = data.frame()
health_impact_layers = list()
counties = subset(pop_exposure_stack, c('County.Island', 'County.Jefferson', 'County.San.Juan', 'County.Skagit', 'County.Snohomish'))
for (county in names(counties)) {
  county_name = gsub('\\.', ' ', gsub('County.', '', county))
  message(county_name)
  
  # Total county population
  county_pop = counties[[county]]
  npop_county = cellStats(county_pop, 'sum')
  message('  Total        ', npop_county)
  
  # Adverse health exposure
  dasy_pop_cropped = crop(county_pop, pop_exposure_stack[['Ldn']])
  dasy_pop_masked = mask(dasy_pop_cropped, pop_exposure_stack[['Ldn']])
  dasy_pop_masked[dasy_pop_masked == 0] = NA # set all 0 population cells to NA
  estimated_pop_exposed = dasy_pop_masked
  npop_exposed = cellStats(estimated_pop_exposed, 'sum')
  message('  Exposed      ', npop_exposed)
  
  ## Annoyance
  # Multiply population in each cell by %HA to yield estimate of # highly annoyed persons in that cell
  estimated_pop_HA_ISO = percent_HA_ISO * 0.01 * county_pop
  npop_HA_ISO = cellStats(estimated_pop_HA_ISO, 'sum')
  message('  HA ISO       ', npop_HA_ISO)
  estimated_pop_HA_WHO = percent_HA_WHO * 0.01 * county_pop
  npop_HA_WHO = cellStats(estimated_pop_HA_WHO, 'sum')
  message('  HA WHO       ', npop_HA_WHO)
  estimated_pop_HA_Yokoshima = percent_HA_Yokoshima * 0.01 * county_pop
  npop_HA_Yokoshima = cellStats(estimated_pop_HA_Yokoshima, 'sum')
  message('  HA Yokoshima ', npop_HA_Yokoshima)
  
  ## Sleep disturbance
  # Multiply population in each cell by %HSD to yield estimate of # sleep-disturbed persons in that cell
  estimated_pop_HSD = percent_HSD * 0.01 * county_pop
  npop_HSD = cellStats(estimated_pop_HSD, 'sum')
  message('  HSD          ', npop_HSD)
  
  ## Hearing impairment
  # Sum population cells exposed to levels causing hearing impairment over time
  estimated_pop_HL = mask(county_pop, area_Leq24_HL)
  npop_HL = cellStats(estimated_pop_HL, 'sum')
  message('  HL           ', npop_HL)
  
  # Add county results to table
  health_impact_summary = rbind(health_impact_summary, data.frame(
    County       = county_name,
    Population   = npop_county,
    Exposed      = npop_exposed,
    HA_ISO       = npop_HA_ISO,
    HA_WHO       = npop_HA_WHO,
    HA_Yokoshima = npop_HA_Yokoshima,
    HSD          = npop_HSD,
    HL           = npop_HL
  ))
  
  # Add county health impact layers to stack
  names(estimated_pop_exposed) = glue('{county_name}.Exposed')
  names(estimated_pop_HA_ISO) = glue('{county_name}.HA.ISO')
  names(estimated_pop_HA_WHO) = glue('{county_name}.HA.WHO')
  names(estimated_pop_HA_Yokoshima) = glue('{county_name}.HA.Yokoshima')
  names(estimated_pop_HSD) = glue('{county_name}.HSD')
  names(estimated_pop_HL) = glue('{county_name}.HL')
  health_impact_layers = append(health_impact_layers, list(
    estimated_pop_exposed,
    estimated_pop_HA_ISO,
    estimated_pop_HA_WHO,
    estimated_pop_HA_Yokoshima,
    estimated_pop_HSD,
    estimated_pop_HL
  ))
}

# Check values
stopifnot(sum(health_impact_summary$Exposed) == cellStats(pop_exposure_stack[['Impacted.Population']], 'sum'))

# Percent exposed per county
data.frame(
  health_impact_summary$County,
  PercentExposed = health_impact_summary$Exposed / health_impact_summary$Population
)

# Format table and calculate totals for the entire study region
health_impact_summary = health_impact_summary %>% mutate_at(c(2:ncol(health_impact_summary)), round)
health_impact_summary = health_impact_summary[order(health_impact_summary$Exposed, decreasing=T), ]
health_impact_summary = health_impact_summary[!(health_impact_summary$County %in% c('Snohomish')), ] # remove Snohomish (no exposure)
health_impact_summary = rbind(health_impact_summary, c(
  'Total',
  sum(health_impact_summary$Population),
  sum(health_impact_summary$Exposed),
  sum(health_impact_summary$HA_ISO),
  sum(health_impact_summary$HA_WHO),
  sum(health_impact_summary$HA_Yokoshima),
  sum(health_impact_summary$HSD),
  sum(health_impact_summary$HL)
))
health_impact_summary

# Write table to file
filename = glue(output_path, '/health_impact_summary.csv')
write.csv(health_impact_summary, filename, row.names = F)
message('Created ', filename)

# Write health impact layers to file
health_impact_stack = stack(health_impact_layers)
filename = glue('{output_path}/health_impact_stack.grd')
writeRaster(brick(health_impact_stack), filename = filename, overwrite = T)
message('Created ', filename)

### Other insights

## Number of people exposed to levels beyond bounds of ERFs
# Annoyance (75, ISO and WHO)
HA_exceed = round(cellStats(mask(pop_exposure_stack[['Impacted.Population']], clamp(pop_exposure_stack[['Ldn']], lower=bounds_who[2], useValues=F)), 'sum'))
# Sleep disturbance (65, ISO/Smith)
HSD_exceed = round(cellStats(mask(pop_exposure_stack[['Impacted.Population']], clamp(pop_exposure_stack[['Lnight']], lower=bounds_HSD[2], useValues=F)), 'sum'))

## Number of people exposed to sleeping disturbance risk threshold
HSD_exposed = round(cellStats(mask(pop_exposure_stack[['Impacted.Population']], clamp(pop_exposure_stack[['Lnight']], lower=lnight_impact_threshold, useValues=F)), 'sum'))

## Number of people exposed to noise levels incompatible with residential land use (65 dB Ldn, FAA and HUD)
residential_exposed = round(cellStats(mask(pop_exposure_stack[['Impacted.Population']], clamp(pop_exposure_stack[['Ldn']], lower=residential_impact_threshold, useValues=F)), 'sum'))
