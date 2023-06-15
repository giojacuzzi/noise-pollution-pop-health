source('global.R')

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

# NOTE: shapefiles validated to remove intersections via https://mapshaper.org/

get_contours_Ldn = function() {
  return(get_contours('data/flight_ops/modeling/baseops/Aggregated/DNL/NASWI_Aggregated_Noisemap - Aggregate_ContourLine_Lines - VALID/NASWI_Aggregated_Noisemap - Aggregate_ContourLine_Lines.shp'))
}

get_contours_Lnight = function() {
  return(get_contours('data/flight_ops/modeling/baseops/Aggregated/DNL_NIGHT/NASWI_Aggregated_Noisemap_NIGHT - Aggregate_ContourLine_Lines - VALID/NASWI_Aggregated_Noisemap_NIGHT - Aggregate_ContourLine_Lines.shp'))
}

get_contours_Leq24 = function() {
  return(get_contours('data/flight_ops/modeling/baseops/Aggregated/LEQ24/NASWI_Aggregated_Noisemap_LEQ - Aggregate_ContourLine_Lines - VALID/NASWI_Aggregated_Noisemap - Aggregate_ContourLine_Lines_LEQ24_ContourLine_Lines.shp'))
}

get_flighttracks = function() {
  flighttracks = st_read('data/flight_ops/modeling/baseops/Aggregated/DNL/NASWI_Aggregated_Noisemap - Aggregate_ContourLine_Lines - VALID/_FlightTracks_Lines.shp')
  return(st_set_crs(flighttracks, crs))
}

get_runways = function() {
  runways = st_read('data/flight_ops/modeling/baseops/Aggregated/DNL/NASWI_Aggregated_Noisemap - Aggregate_ContourLine_Lines - VALID/_Runways_Lines.shp')
  return(st_set_crs(runways, crs))
}

if (!exists('contours_Ldn') | !exists('contours_Lnight') | !exists('contours_Leq24')) {
  contours_Ldn    = get_contours_Ldn()
  contours_Lnight = get_contours_Lnight()
  contours_Leq24  = get_contours_Leq24()
  
  mapview(get_flighttracks(), lwd = 0.5) + mapview(get_runways(), lwd = 5) +
    mapview(contours_Ldn,    zcol='Level') +
    mapview(contours_Lnight, zcol='Level') +
    mapview(contours_Leq24,  zcol='Level')
}
