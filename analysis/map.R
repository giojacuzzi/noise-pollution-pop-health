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

###################################################################################################
# Lnight - (Highly sleep disturbed)
contours_Lnight = get_contours(path_Lnight)
mapview(contours_Lnight, zcol = 'Level') + mapview(wa_bg_population) # overlap

# Full map
intersection_Lnight = st_intersection(wa_bg_population[,c('GEOID', 'NAME', 'value', 'geometry')], contours_Lnight)
mapview(intersection_Lnight, zcol='Level', layer.name='DNL') + mapview(wa_bg_population, col.regions=list('white'))

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

mapview(intersection_DNL, zcol='pop_prop', layer.name='Population') +
  mapview(intersection_Lnight, zcol='pop_HSD', at=seq(0, 350, 50), layer.name='Population HSD')

########################################################################################################
# Insights

# Number of people estimated to be exposed to >= 70 dB DNL...
# using 'block' ~ 8238, 'block group' ~ 9072, 'tract' ~ 8542
sum(st_drop_geometry(intersection_DNL[intersection_DNL$Level>=70, ])$pop_prop)

# Number of people estimated to be highly annoyed according to WHO guidelines...
# using 'block' ~ 19745, 'block group' ~ 20626, 'tract' ~ 20616
sum(st_drop_geometry(intersection_DNL)$pop_HA_WHO)

# Number of people estimated to be highly sleep disturbed according to WHO (expanded) guidelines...
# using 'block' ~ 19745, 'block group' ~ 20626, 'tract' ~ 20616
sum(st_drop_geometry(intersection_Lnight)$pop_HSD)
