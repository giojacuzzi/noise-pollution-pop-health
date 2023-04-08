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

crs = 'NAD83'

sites = st_as_sf(get_data_sites(),
                 coords = c('Longitude', 'Latitude'), 
                 crs = crs, agr = 'constant')
sites = na.omit(sites)
sites = sites[sites$ID %in% unique(get_data_metrics()[,'ID']), ]
sites$Longitude = st_coordinates(sites$geometry)[,'X']
sites$Latitude  = st_coordinates(sites$geometry)[,'Y']

bounds_x = c(-123.8, -121.2) # [min, max]
bounds_y = c(47.6, 48.7)
bounds = data.frame(
  x = c(bounds_x[1], bounds_x[1], bounds_x[2], bounds_x[2]),
  y = c(bounds_y[1], bounds_y[2], bounds_y[2], bounds_y[1])
)

wa_map = ggplot() +
  geom_sf(data = wa_counties_cb) +
  geom_polygon(data = bounds, aes(x, y, group = 1), fill=NA, color = 'red')
print(wa_map)

sf_extSoftVersion()

# Shapefiles validated to remove intersections via https://mapshaper.org/
# path = 'data/flight_ops/modeling/baseops/Aggregated/DNL/NASWI_Aggregated_Noisemap - Aggregate_ContourLine_Lines - VALID/NASWI_Aggregated_Noisemap - Aggregate_ContourLine_Lines.shp'
path = 'data/flight_ops/modeling/baseops/Aggregated/DNL_NIGHT/NASWI_Aggregated_Noisemap_NIGHT - Aggregate_ContourLine_Lines - VALID/NASWI_Aggregated_Noisemap_NIGHT - Aggregate_ContourLine_Lines.shp'
# TODO: account for DNL nighttime penalties
shp_contours = st_read(path)
if (is.na(st_crs(shp_contours))) st_crs(shp_contours) = crs
st_is_longlat(shp_contours)
ggplot() + geom_sf(data = shp_contours)

# Default overlapping contours (i.e. 65 dB contour encapsulates contours of all levels >= 65)
contours_polygon_overlap = sf::st_cast(shp_contours, "MULTIPOLYGON")
contours_polygon_overlap$Level = seq(from=10, by=5, length.out=nrow(contours_polygon_overlap))

contours_polygon_overlap <- contours_polygon_overlap %>% dplyr::group_by(Level)

# st_is_valid(contours_polygon_overlap, reason = TRUE)
contours_polygon_overlap = st_make_valid(contours_polygon_overlap)

plot(contours_polygon_overlap)

# Separate contours per level
contours_polygon = st_make_valid(contours_polygon_overlap)
for (r in 1:nrow(contours_polygon)) {
  target = contours_polygon[r,]
  level = target$Level
  if (r < nrow(contours_polygon)) {
    others = st_union(contours_polygon[which(contours_polygon$Level>level),])
    diff = st_difference(st_geometry(target), st_geometry(others))
    st_geometry(target) = st_geometry(diff)
  }
  st_geometry(contours_polygon[r,]) = st_geometry(target)
  contours_polygon[r,] = st_as_sf(contours_polygon[r,])
  contours_polygon = st_as_sf(contours_polygon)

  # p = ggplot() +
  #   geom_sf(data = contours_polygon[r,], aes(fill = Level)) +
  #   labs(title = level)
  # print(p)
}

library(ggrepel)
area_map = ggplot() +
  geom_sf(data = wa_population) + # aes(fill = value)
  geom_sf(data = sites, size = 1, shape = 8, color = 'red') +
  # geom_text_repel(data = sites,
  #                 aes(x = Longitude, y = Latitude, label = ID),
  #                 size = 2, col = 'black', fontface = 'bold', max.overlaps = 30,
  #                 nudge_x = c(),
  #                 nudge_y = c()) +
  geom_sf(data = contours_polygon[contours_polygon$Level>=30,],
          aes(fill = Level), alpha=1.0) +
  scale_fill_viridis(option="magma") +
  coord_sf(xlim = bounds_x, ylim = bounds_y)
print(area_map)

## Noise complaint reports
data_reports = get_data_complaint_reports()
data_reports = data_reports[!is.na(data_reports$Longitude) & !is.na(data_reports$Longitude),]
sf_reports = st_as_sf(data_reports,
                 coords = c('Longitude', 'Latitude'),
                 crs = crs, agr = 'constant')
# sf_reports = na.omit(sf_reports)
# reports = reports[reports$ID %in% unique(get_data_metrics()[,'ID']), ]
sf_reports$Longitude = st_coordinates(sf_reports$geometry)[,'X']
sf_reports$Latitude  = st_coordinates(sf_reports$geometry)[,'Y']

map_reports = ggplot() +
  geom_sf(data = wa_population) + # aes(fill = value)
  geom_sf(data = sf_reports, size = 1, shape = 8, color = 'red') +
  coord_sf(xlim = bounds_x, ylim = bounds_y)
print(map_reports)

# TODO: reports for only the Navy monitoring weeks
