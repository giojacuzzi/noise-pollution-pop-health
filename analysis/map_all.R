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

input_path = paste0(here::here(), '/analysis/_output')
output_path = paste0(here::here(), '/analysis/_output')
pop_exposure_stack = stack(glue('{input_path}/pop_exposure_stack.grd'))
pop_HA_stack = stack(glue('{input_path}/pop_HA_stack.grd'))
pop_HL_stack = stack(glue('{input_path}/pop_HL_stack.grd'))
pop_HSD_stack = stack(glue('{input_path}/pop_HSD_stack.grd'))
pop_stack = stack(pop_exposure_stack, pop_HA_stack, pop_HL_stack, pop_HSD_stack)

# Run all the other scripts first
mapview(get_flighttracks()) +
  mapview(pop_exposure_stack[['Ldn']], layer.name=c('Ldn (dB)')) +
  mapview(pop_exposure_stack[['Impacted.Population']], layer.name=c('Impacted Persons')) +
  mapview(estimated_pop_HA_WHO, layer.name=c('Persons Highly Annoyed (WHO)')) + # at=seq(0,5,1)
  mapview(estimated_pop_HA_Yokoshima, layer.name=c('Persons Highly Annoyed (Yokoshima)')) + # at=seq(0,5,1)
  mapview(pop_exposure_stack[['Lnight']], layer.name=c('Lnight (dB)')) +
  mapview(estimated_pop_HSD, layer.name=c('Persons Highly Sleep Disturbed (WHO)')) +
  mapview(pop_exposure_stack[['Leq24']], layer.name=c('Leq24 (dB)')) +
  mapview(estimated_pop_hearing_loss, layer.name=c('Persons Hearing Loss (EPA)')) +
  mapview(schools, zcol='Ldn55', col.regions=list('gray', 'red'), layer.name=c('Impacted Schools'))

# Get 2021 ACS 5-year block group population estimates
acs_pop = get_acs(geography = 'county', variables = 'B01003_001', year = 2021, state = 'WA', geometry = T)
acs_pop = st_transform(acs_pop, st_crs(pop_exposure_stack[['Impacted.Population']]))

counties = c(
  "Jefferson County, Washington",
  "San Juan County, Washington" ,
  "Snohomish County, Washington",
  "Island County, Washington",
  "Skagit County, Washington"
)
county_results = data.frame()
for (County in counties) {
  message(County)
  Exposed = mask(pop_stack[['Impacted.Population']], as(acs_pop[acs_pop$NAME == County,], 'Spatial'))
  HA_ISO = mask(pop_stack[['HA_ISO']], as(acs_pop[acs_pop$NAME == County,], 'Spatial'))
  HA_WHO = mask(pop_stack[['HA_WHO']], as(acs_pop[acs_pop$NAME == County,], 'Spatial'))
  HA_Yokoshima = mask(pop_stack[['HA_Yokoshima']], as(acs_pop[acs_pop$NAME == County,], 'Spatial'))
  HL = mask(pop_stack[['HL']], as(acs_pop[acs_pop$NAME == County,], 'Spatial'))
  HSD = mask(pop_stack[['HSD']], as(acs_pop[acs_pop$NAME == County,], 'Spatial'))
  county_results = rbind(county_results, data.frame(
    County,
    Exposed      = cellStats(Exposed, 'sum'),
    HA_ISO       = cellStats(HA_ISO, 'sum'),
    HA_WHO       = cellStats(HA_WHO, 'sum'),
    HA_Yokoshima = cellStats(HA_Yokoshima, 'sum'),
    HL           = cellStats(HL, 'sum'),
    HSD          = cellStats(HSD, 'sum')
  ))
}
county_results = rbind(county_results, c(
  'Total',
  sum(county_results$Exposed),
  sum(county_results$HA_ISO),
  sum(county_results$HA_WHO),
  sum(county_results$HA_Yokoshima),
  sum(county_results$HL),
  sum(county_results$HSD)
))

stopifnot(sum(county_results$Exposed) == cellStats(pop_exposure_stack[['Impacted.Population']], 'sum'))

npop_HA_Yokoshima
npop_HA_WHO
npop_HA_ISO
npop_HSD
npop_hearing_loss