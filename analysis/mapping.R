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
# census_api_key('a9d9f05e0560c7fadaed6b4168bedc56d0e4686d')

wa_population = get_decennial(
  geography = 'county',
  variables = 'P1_001N',
  state = 'WA',
  year = 2020,
  geometry = TRUE
)

plot(wa_population['value'])

library(ggplot2)
wa_map = ggplot(wa_population, aes(fill = value)) + geom_sf()
print(wa_map)
