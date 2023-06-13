# Map HA and HSD
library(dplyr)
library(sf)
library(mapview)
library(ggplot2)
library(ggrepel)
library(tidycensus)
census_api_key('a9d9f05e0560c7fadaed6b4168bedc56d0e4686d')
sf_extSoftVersion()

source('analysis/exposure_response_functions.R')

crs = 'NAD83'
wa_bg_population = get_decennial(
  geography = 'block group',
  variables = 'P1_001N', # population
  state = 'WA',
  year = 2020,
  geometry = TRUE
)
mapview(wa_bg_population, zcol = 'value')

# Read and process noise contours from shapefile at path
get_contours = function(path) {
  shp_contours = st_read(path)
  
  # Default overlapping contours (i.e. 65 dB contour encapsulates contours of all levels >= 65)
  contours_polygon_overlap = st_cast(shp_contours, 'MULTIPOLYGON')
  contours_polygon_overlap$Level = seq(from=10, by=5, length.out=nrow(contours_polygon_overlap))
  contours_polygon_overlap = contours_polygon_overlap %>% group_by(Level)
  
  if (nrow(contours_polygon_overlap) != sum(st_is_valid(contours_polygon_overlap)))
    stop('Contours not valid!')
  contours_polygon_overlap = st_make_valid(contours_polygon_overlap)
  
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
    # p = ggplot() + geom_sf(data = contours_polygon[r,], aes(fill = Level)) + labs(title = level); print(p)
  }
  return(st_transform(contours_polygon, crs))
}

# Shapefiles validated to remove intersections via https://mapshaper.org/
path_DNL = 'data/flight_ops/modeling/baseops/Aggregated/DNL/NASWI_Aggregated_Noisemap - Aggregate_ContourLine_Lines - VALID/NASWI_Aggregated_Noisemap - Aggregate_ContourLine_Lines.shp'
path_Lnight = 'data/flight_ops/modeling/baseops/Aggregated/DNL_NIGHT/NASWI_Aggregated_Noisemap_NIGHT - Aggregate_ContourLine_Lines - VALID/NASWI_Aggregated_Noisemap_NIGHT - Aggregate_ContourLine_Lines.shp'
path_leq24 = 'data/flight_ops/modeling/baseops/Aggregated/LEQ24/NASWI_Aggregated_Noisemap_LEQ - Aggregate_ContourLine_Lines - VALID/NASWI_Aggregated_Noisemap - Aggregate_ContourLine_Lines_LEQ24_ContourLine_Lines.shp'

###################################################################################################
# DNL - (General exposure, annoyance, etc.)
contours_DNL = get_contours(path_DNL)
mapview(contours_DNL, zcol = 'Level') + mapview(wa_bg_population) # overlap

# Example with Coupeville block group
ex_bg = wa_bg_population[wa_bg_population$GEOID==530299710001,]
ex_intersection = st_intersection(ex_bg, contours_DNL)
mapview(ex_intersection, zcol='Level')
areas = as.numeric(st_area(ex_intersection) / st_area(ex_bg)) # proportion
ex_intersection$population = ex_intersection$value * areas
mapview(ex_intersection, zcol='population')
level_areas = left_join(data.frame(Level=contours_DNL$Level), data.frame(Level=ex_intersection$Level, Area=areas), by = join_by(Level)) %>% replace(is.na(.), 0)

# Full map
intersection_DNL = st_intersection(wa_bg_population[,c('GEOID', 'NAME', 'value', 'geometry')], contours_DNL)
mapview(intersection_DNL, zcol='Level', layer.name='DNL') + mapview(wa_bg_population, col.regions=list('white'))

# population total for an intersection feature, estimated as proportion of total block group population
intersection_DNL$pop_prop = 0
for (r in 1:nrow(intersection_DNL)) {
  i = intersection_DNL[r,]
  bg = wa_bg_population[wa_bg_population$GEOID==i$GEOID,]
  proportion = as.numeric(st_area(i) / st_area(bg))
  intersection_DNL[r, 'pop_prop'] = round(proportion * bg$value) # proportion of total block group population
}

# Multiply population in each block group intersection by %HA to yield estimate of # highly annoyed persons
intersection_DNL$pop_HA_WHO = round(sapply(as.numeric(intersection_DNL$Level), exp_resp_WHO_bounded) * 0.01 * intersection_DNL$pop_prop)
intersection_DNL$pop_HA_ISO_Miedema = round(sapply(as.numeric(intersection_DNL$Level), exp_resp_ISO_Miedema_bounded) * 0.01 * intersection_DNL$pop_prop)
intersection_DNL$pop_HA_Yokoshima = round(sapply(as.numeric(intersection_DNL$Level), exp_resp_Yokoshima_bounded) * 0.01 * intersection_DNL$pop_prop)

breaks = seq(0, 800, 100)
mapview(intersection_DNL, zcol='pop_prop', layer.name='Population') +
  mapview(intersection_DNL, zcol='pop_HA_WHO', at=breaks, layer.name='Population HA (WHO)') +
  mapview(intersection_DNL, zcol='pop_HA_ISO_Miedema', at=breaks, layer.name='Population HA (ISO)') +
  mapview(intersection_DNL, zcol='pop_HA_Yokoshima', at=breaks, layer.name='Population HA (Yokoshima)')

########################################################################################################
# Children's learning and comprehension

# DNL >= 55 dB poses risk of inhibited reading skills and oral comprehension in children (WHO, RANCH)
DNL_gt55 = intersection_DNL[as.numeric(intersection_DNL$Level)>=55,]
DNL_gt55_bounds = st_bbox(DNL_gt55) #intersection_DNL

# https://nces.ed.gov/programs/edge/geographic/schoollocations
dir = '/Volumes/SAFS Work/PHI/Schools'
schools_public  = st_read(paste0(dir, '/EDGE_GEOCODE_PUBLICSCH_2021/Shapefile/EDGE_GEOCODE_PUBLICSCH_2021.shp'))
schools_private = st_read(paste0(dir, '/EDGE_GEOCODE_PRIVATESCH_1920/Shapefile/EDGE_GEOCODE_PRIVATESCH_1920.shp'))
schools_postsec = st_read(paste0(dir, '/EDGE_GEOCODE_POSTSECONDARYSCH_2021/Shapefile/EDGE_GEOCODE_POSTSECSCH_2021.shp'))
schools_public  = st_transform(schools_public, crs)
schools_private = st_transform(schools_private, crs)
schools_postsec = st_transform(schools_postsec, crs)

schools_public = st_crop(schools_public, DNL_gt55_bounds)
schools_private = st_crop(schools_private, DNL_gt55_bounds)
schools_postsec = st_crop(schools_postsec, DNL_gt55_bounds)

schools_public  = schools_public[, c('NAME', 'ZIP', 'LAT', 'LON')]
schools_public$TYPE = 'PUBLIC'
schools_private = schools_private[, c('NAME', 'ZIP', 'LAT', 'LON')]
schools_private$TYPE = 'PRIVATE'
schools_postsec = schools_postsec[, c('NAME', 'ZIP', 'LAT', 'LON')]
schools_postsec$TYPE = 'POSTSECONDARY'

schools = bind_rows(bind_rows(schools_public, schools_private), schools_postsec)
schools$TYPE = factor(schools$TYPE)
schools_affected = st_intersection(schools, DNL_gt55) # affected schools
schools_affected$DNL55 = TRUE
schools_unaffected = schools[!(schools$NAME %in% schools_affected$NAME),]
schools_unaffected$DNL55 = FALSE
schools = bind_rows(schools_affected, schools_unaffected)

mapview(DNL_gt55, zcol='Level', layer.name='DNL') + mapview(wa_bg_population, col.regions=list('white')) + mapview(schools, zcol='DNL55', col.regions=list('gray', 'red'))

###################################################################################################
# Lnight - (Highly sleep disturbed)
contours_Lnight = get_contours(path_Lnight)
mapview(contours_Lnight, zcol = 'Level') + mapview(wa_bg_population) # overlap

# Full map
intersection_Lnight = st_intersection(wa_bg_population[,c('GEOID', 'NAME', 'value', 'geometry')], contours_Lnight)
mapview(intersection_Lnight, zcol='Level', layer.name='Lnight') + mapview(wa_bg_population, col.regions=list('white'))

# population total for an intersection feature, estimated as proportion of total block group population
intersection_Lnight$pop_prop = 0
for (r in 1:nrow(intersection_Lnight)) {
  i = intersection_Lnight[r,]
  bg = wa_bg_population[wa_bg_population$GEOID==i$GEOID,]
  proportion = as.numeric(st_area(i) / st_area(bg))
  intersection_Lnight[r, 'pop_prop'] = round(proportion * bg$value) # proportion of block group pop
}

# Multiply pop in each bg intersection by %HSD to yield estimate of # highly sleep-disturbed persons
intersection_Lnight$pop_HSD = round(sapply(as.numeric(intersection_Lnight$Level), exp_resp_HSD_combinedestimate_bounded) * 0.01 * intersection_Lnight$pop_prop)

mapview(intersection_Lnight, zcol='pop_prop', layer.name='Population') +
  mapview(intersection_Lnight, zcol='pop_HSD', at=seq(0, 350, 50), layer.name='Population HSD')

###################################################################################################
# Leq24
contours_leq24 = get_contours(path_leq24)
mapview(contours_leq24, zcol = 'Level') + mapview(wa_bg_population) # overlap

# Full map
intersection_leq24 = st_intersection(wa_bg_population[,c('GEOID', 'NAME', 'value', 'geometry')], contours_leq24)
mapview(intersection_leq24, zcol='Level', layer.name='Leq24') + mapview(wa_bg_population, col.regions=list('white'))

# population total for an intersection feature, estimated as proportion of total block group population
intersection_leq24$pop_prop = 0
for (r in 1:nrow(intersection_leq24)) {
  i = intersection_leq24[r,]
  bg = wa_bg_population[wa_bg_population$GEOID==i$GEOID,]
  proportion = as.numeric(st_area(i) / st_area(bg))
  intersection_leq24[r, 'pop_prop'] = round(proportion * bg$value) # proportion of block group pop
}

# population total for an intersection feature, estimated as proportion of total block group population
intersection_leq24$pop_prop = 0
for (r in 1:nrow(intersection_leq24)) {
  i = intersection_leq24[r,]
  bg = wa_bg_population[wa_bg_population$GEOID==i$GEOID,]
  proportion = as.numeric(st_area(i) / st_area(bg))
  intersection_leq24[r, 'pop_prop'] = round(proportion * bg$value) # proportion of block group pop
}

mapview(intersection_leq24[intersection_leq24$Level>=70, ], zcol='pop_prop', layer.name='Population 70+ dB Leq24')

########################################################################################################
# Insights
# Note that these numbers may be conservative estimates due to the lack of evening-time penalty in DNL calculations, and depending on the exposure-response function used.

# WHO - "For average noise exposure, the GDG strongly recommends reducing noise levels produced by aircraft below 45 dB Lden, as aircraft noise above this level is associated with adverse health effects.
mapview(intersection_DNL[as.numeric(intersection_DNL$Level)>=45,], zcol='Level') + mapview(wa_bg_population, col.regions=list('white'))
print(ggplot() +
  geom_sf(data = wa_bg_population) + # aes(fill = value)
  geom_sf(data = sf_reports, size = 1, shape = 19, color = 'red', alpha=0.1)
)

# WHO - "For night noise exposure, the GDG strongly recommends reducing noise levels produced by aircraft during night time below 40 dB Lnight, as night- time aircraft noise above this level is associated with adverse effects on sleep."
mapview(intersection_Lnight[as.numeric(intersection_Lnight$Level)>=45,], zcol='Level') + mapview(wa_bg_population, col.regions=list('white'))

# Number of people estimated to be exposed to >= 70 dB Leq24 (EPA hearing loss over time)
sum(st_drop_geometry(intersection_leq24[intersection_leq24$Level>=70, ])$pop_prop)

# Number of people estimated to be highly annoyed according to WHO guidelines...
# using 'block' ~ 19745, 'block group' ~ 20626, 'tract' ~ 20616
# ...and ISO, Yokoshima
sum(st_drop_geometry(intersection_DNL)$pop_HA_WHO)
sum(st_drop_geometry(intersection_DNL)$pop_HA_ISO_Miedema)
sum(st_drop_geometry(intersection_DNL)$pop_HA_Yokoshima)

# Number of people estimated to be highly sleep disturbed according to WHO (Smith expanded) guidelines...
sum(st_drop_geometry(intersection_Lnight)$pop_HSD)

########################################################################################################
## Noise complaint reports
source('global.R')

data_reports = get_data_complaint_reports()
data_reports = data_reports[!is.na(data_reports$Longitude) & !is.na(data_reports$Longitude),]
sf_reports = st_as_sf(data_reports,
                      coords = c('Longitude', 'Latitude'),
                      crs = crs, agr = 'constant')
sf_reports$Longitude = st_coordinates(sf_reports$geometry)[,'X']
sf_reports$Latitude  = st_coordinates(sf_reports$geometry)[,'Y']

map_reports = ggplot() +
  geom_sf(data = wa_bg_population) + # aes(fill = value)
  geom_sf(data = sf_reports, size = 1, shape = 19, color = 'red', alpha=0.1)
print(map_reports)

mapview(sf_reports, zcol='Character')
mapview(sf_reports) + mapview(intersection_DNL, zcol='Level')

# TODO: cull reports over water
# TODO: reports for only the Navy monitoring weeks
