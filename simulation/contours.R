source('global.R')
source('metrics/health_metrics.R')
library(glue)

# Read and process noise contours from shapefile at path
get_contours = function(metric) {

  if (!(metric %in% c('DNL', 'LEQ24', 'LNIGHT')))
    stop('Invalid metric requested')
  
  path = glue('simulation/_output/{metric}/{metric}_ContourLine_Lines.shp')
  shp_contours = st_read(path, quiet = T)
  
  # Default overlapping contours (i.e. 65 dB contour encapsulates contours of all levels >= 65)
  contours_polygon_overlap = st_cast(shp_contours, 'MULTIPOLYGON')
  contours_polygon_overlap$Level = seq(from=10, by=5, length.out=nrow(contours_polygon_overlap))
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

get_contours_Ldn = function() { return(get_contours('DNL')) }
get_contours_Leq24 = function() { return(get_contours('LEQ24')) }
get_contours_Lnight = function() { return(get_contours('LNIGHT')) }

# Determine which counties are within the spatial distribution of noise health impacts
get_exposed_counties = function() {
  counties = list()
  contours = list(get_contours_Ldn(), get_contours_Leq24(), get_contours_Lnight())
  c = 1
  for (contour in contours) {
    if (c == 1) l = lden_impact_threshold
    if (c == 2) l = HL_leq24_impact_threshold
    if (c == 3) l = lnight_impact_threshold
    
    contour = contour[as.numeric(contour$Level)>=l,]
    
    stid = 'WA'
    pop = get_acs(geography = 'county', variables = 'B01003_001', year = 2021, state = 'WA', geometry = TRUE)
    # mapview(contour) + mapview(pop)
    
    st_agr(pop) = 'constant'
    st_agr(contour) = 'constant'
    intersection = st_intersection(pop, contour)
    counties = append(counties, unique(intersection$NAME))
    c = c+1
  }
  counties = unlist(unique(counties))
  counties = unlist(lapply(counties, function(s) { unlist(str_split(s, ' County, Washington'))[1] }))
  return(counties)
}
