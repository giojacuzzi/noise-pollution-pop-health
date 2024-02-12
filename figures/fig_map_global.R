## Global variables used in map generation

library(patchwork)
library(ggsn)
library(tigris)
options(tigris_use_cache = T)

source('global.R')

output_path = paste0(here::here(), '/figures/_output')

# Label locations

labels_large = data.frame(
  Name      = c('Oak Harbor', 'Coupeville', 'Port Townsend', 'Anacortes', 'Camano Island', 'La Conner'),
  Lat  = c(48.297324,   48.213824,   48.125762,   48.502063, 48.2415588,   48.389814),
  Lon = c(-122.659911, -122.675396, -122.799917, -122.620023, -122.490305, -122.470759)
)

labels_medium = data.frame(
  Name = c('Swinomish\nReservation', 'Samish TDSA'),
  Lat  = c(48.424, 48.452),
  Lon  = c(-122.529, -122.593)
)

labels_small = data.frame(
  Name = c('Lopez Island'),
  Lat  = c(48.438),
  Lon  = c(-122.835)
)

# Colors

colors_native = c('#11FF0044','#FFFF0044')
color_military = '#FF000044'

# Spatial objects
if (!exists('wa_counties_cb')) {
  wa_counties_cb = counties(state = 'WA', cb = T)
  wa_counties_cb = st_transform(wa_counties_cb, 'WGS84')
}

if (!exists('wa_roads')) {
  wa_roads = c()
  for (c in c('Island', 'Jefferson', 'San Juan', 'Skagit', 'Snohomish', 'Clallam')) {
    wa_roads = rbind(wa_roads, roads(state = 'WA', county = c))
  }
}

if (!exists('wa_military')) {
  wa_military = st_crop(military(year = 2021), xmin = -122.86, ymin = 48.09, xmax = -122.33, ymax = 48.47)
  wa_military = wa_military[wa_military$FULLNAME %in% c('Naval Air Station Whidbey Island', 'Naval Outlying Field Coupeville'), ]
}

if (!exists('naswi_water')) {
  naswi_water = c()
  for (c in c('Island', 'Jefferson', 'San Juan', 'Skagit', 'Snohomish', 'Clallam')) {
    naswi_water = rbind(naswi_water, st_transform(area_water(state = 'WA', county = c), 'WGS84'))
  }
  naswi_water = st_union(naswi_water)
}

if (!exists('naswi_land')) {
  naswi_land = st_union(wa_counties_cb)
  naswi_land = st_difference(naswi_land, naswi_water)
}

if (!exists('native_areas')) {
  native_areas = get_acs(geography = 'american indian area/alaska native area/hawaiian home land', variables = 'B01003_001', year = 2021, geometry = T)
  native_areas = native_areas[native_areas$GEOID %in% c(8750,4075),] # Samish, Swinomish
  native_areas = st_intersection(st_transform(native_areas, st_crs(naswi_land)), st_make_valid(naswi_land)) 
}

runways = st_read('data/gis/NASWI/NASWI_Runways_Lines.shp', quiet = T)
runways = st_set_crs(runways, 'WGS84')
