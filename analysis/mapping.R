## Noise exposure spatial distribution
# US Census Geography Hierarchy: Nation > Region > Division > State > County > Census Tract > Block Group > Block (+ AIANNH/Tribal Areas)
# Legal entities (e.g. counties), statistical entities (e.g. tracts), geographic features
# Census TIGER/Line shapefiles:
# - Topologically Integrated Geographic Encoding and Referencing database
# - High-quality series of geographic datasets released by US Census Bureau
# - Distributed as shapefiles
# 'tigris' package is an R interfaceto the US Census Bureau's TIGER/Line shapefile FTP server. No API key necessary'
# 'sf' package implements simple features data model for vector spatial data in R, allowing for tidy spatial data analysis

library(tigris)
options(tigris_use_cache = T)

# NOTE: Relevant counties are: San Juan, Skagit, Clallam, Jefferson, Island, Snohomish
counties = c('San Juan', 'Skagit', 'Clallam', 'Jefferson', 'Island', 'Snohomish')

wa_counties = counties(state = 'WA')
plot(wa_counties$geometry)

wa_counties_cb = counties(state = 'WA', cb = T)
plot(wa_counties_cb$geometry)

# Census tracts are "designed to be relatively homogeneous units with respect to population characteristics, economic status, and living conditions" and "average about 4,000 inhabitants"
wa_tracts = tracts(state = 'WA', county = counties, year = 2021)
plot(wa_tracts$geometry)

wa_tracts_cb = tracts(state = 'WA', county = counties, cb = T, year = 2021)
plot(wa_tracts_cb$geometry)

# A Census Block Group is a geographical unit used by the United States Census Bureau which is between the Census Tract and the Census Block. It is the smallest geographical unit for which the bureau publishes sample data, i.e. data which is only collected from a fraction of all households. Typically, Block Groups have a population of 600 to 3,000 people.
wa_blockgroups_cb = block_groups(state = 'WA', county = counties, cb = T)
plot(wa_blockgroups_cb$geometry)

wa_water = area_water(state = 'WA', county = 'Island')
plot(wa_water$geometry)

wa_roads = roads(state = 'WA', county = 'Island')
plot(wa_roads$geometry)

wa_landmarks = landmarks(state = 'WA', type = 'point')
plot(wa_landmarks$geometry)

wa_native = native_areas(year = 2021)
# plot(wa_native$geometry)

wa_tribal_census_tracts = tribal_census_tracts(year = 2021)
# plot(wa_tribal_census_tracts$geometry)

wa_military = military(year = 2021)
# plot(wa_military$geometry)

# Interactive viewing via mapview
library(mapview)

mapview(wa_counties)
mapview(wa_tracts_cb)
mapview(wa_blockgroups_cb)

# tidycensus
library(tidycensus)
census_api_key('a9d9f05e0560c7fadaed6b4168bedc56d0e4686d')
library(ggplot2)
library(sf)
source('global.R')

wa_population = get_decennial(
  geography = 'tract',
  variables = 'P1_001N',
  state = 'WA',
  year = 2020,
  geometry = TRUE
)
plot(wa_population['value'])

sites = st_as_sf(get_data_sites(),
                 coords = c("Longitude", "Latitude"), 
                 crs = 'NAD83', agr = "constant")
sites = na.omit(sites)
sites = sites[sites$ID %in% unique(get_data_metrics()[,'ID']), ]
sites$Longitude = st_coordinates(sites$geometry)[,'X']
sites$Latitude  = st_coordinates(sites$geometry)[,'Y']

bounds_x = c(-123.2, -122.0) # [min, max]
bounds_y = c(48.0, 48.6)
bounds = data.frame(
  x = c(bounds_x[1], bounds_x[1], bounds_x[2], bounds_x[2]),
  y = c(bounds_y[1], bounds_y[2], bounds_y[2], bounds_y[1])
)

wa_map = ggplot() +
  geom_sf(data = wa_counties_cb) +
  geom_polygon(data = bounds, aes(x, y, group = 1), fill=NA, color = 'red')
print(wa_map)

path = 'data/flight_ops/modeling/baseops/Aggregated/NASWI_Aggregated_Noisemap - Aggregate_ContourLine_Lines.shp'
shp_contours = st_read(path)
ggplot() + geom_sf(data = shp_contours)

library(ggrepel)
area_map = ggplot() +
  geom_sf(data = wa_population) + # aes(fill = value)
  geom_sf(data = sites, size = 1, shape = 8, color = 'red') +
  # geom_text_repel(data = sites,
  #                 aes(x = Longitude, y = Latitude, label = ID),
  #                 size = 2, col = 'black', fontface = 'bold', max.overlaps = 30,
  #                 nudge_x = c(),
  #                 nudge_y = c()) +
  coord_sf(xlim = bounds_x, ylim = bounds_y)
print(area_map)
