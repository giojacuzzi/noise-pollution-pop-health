## Utility function to retrieve and process noise contours

source('global.R')
source('metrics/thresholds.R')
library(glue)

# Read and process noise contours from shapefile at path
get_contours = function(metric) {

  if (!(metric %in% c('DNL', 'LEQ24', 'LNIGHT')))
    stop('Invalid metric requested')
  
  path = glue('simulation/_output/{metric}/{metric}_ContourLine_Lines.shp')
  shp_contours = st_read(path, quiet = T)
  
  # Default overlapping contours (i.e. 65 dB contour encapsulates contours of all levels >= 65)
  contours_polygon_overlap = st_cast(shp_contours, 'MULTIPOLYGON')
  contours_polygon_overlap$Level = as.numeric(contours_polygon_overlap$Level)
  contours_polygon_overlap = contours_polygon_overlap %>% group_by(Level)
  
  contours_polygon_overlap = st_make_valid(contours_polygon_overlap)
  st_crs(contours_polygon_overlap) = 'WGS84'
  
  if (nrow(contours_polygon_overlap) != sum(st_is_valid(contours_polygon_overlap)))
    stop('Contours not valid!')
  
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

get_contours_Ldn = function(threshold = 0) {
  contours = get_contours('DNL')
  return(contours[as.numeric(contours$Level)>=threshold, ])
}
get_contours_Leq24 = function(threshold = 0) {
  contours = get_contours('LEQ24')
  return(contours[as.numeric(contours$Level)>=threshold, ])
}
get_contours_Lnight = function(threshold = 0) {
  contours = get_contours('LNIGHT')
  return(contours[as.numeric(contours$Level)>=threshold, ])
}

# Determine which counties and native lands are within the spatial distribution of noise health impacts
get_exposed_areas = function() {
  
  msg('Determining exposed areas...')
  
  pop_cty = get_acs(geography = 'county', variables = 'B01003_001', year = 2021, state = 'WA', geometry = T)
  pop_nl = get_acs(geography = 'american indian area/alaska native area/hawaiian home land', variables = 'B01003_001', year = 2021, geometry = T)
  pop_nl = pop_nl[grep(', WA', pop_nl$NAME), ]
  st_agr(pop_cty) = 'constant'
  st_agr(pop_nl) = 'constant'
  
  counties = list()
  native_lands = list()
  contours = list(get_contours_Ldn(), get_contours_Leq24(), get_contours_Lnight())
  c = 1
  for (contour in contours) {
    if (c == 1) l = threshold_adverse_health_effects_Lden
    if (c == 2) l = threshold_hearing_impairment_Leq24
    if (c == 3) l = threshold_sleep_disturbance_Lnight
    
    contour = contour[as.numeric(contour$Level)>=l,]
    st_agr(contour) = 'constant'
    
    intersection_cty = st_intersection(pop_cty, contour)
    counties = append(counties, unique(intersection_cty$NAME))
    intersection_nl = st_intersection(pop_nl, contour)
    native_lands = append(native_lands, unique(intersection_nl$NAME))
    c = c+1
  }
  counties = unlist(unique(counties))
  counties = unlist(lapply(counties, function(s) { unlist(str_split(s, ' County, Washington'))[1] }))
  native_lands = unlist(unique(native_lands))
  
  msg('Exposed counties:', counties)
  msg('Exposed native lands:', native_lands)
  
  return(list(
    counties=counties,
    native_lands=native_lands
  ))
}
