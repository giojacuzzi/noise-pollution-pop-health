# Map HA and HSD
source('global.R')

########################################################################################################
# General map with sites and other features
sites = st_as_sf(get_data_sites(),
                 coords = c('Longitude', 'Latitude'), 
                 crs = crs, agr = 'constant')
sites = na.omit(sites)
sites = sites[sites$ID %in% unique(get_data_metrics()[,'ID']), ]
sites$Longitude = st_coordinates(sites$geometry)[,'X']
sites$Latitude  = st_coordinates(sites$geometry)[,'Y']

flighttracks = st_read('data/flight_ops/modeling/baseops/Aggregated/DNL/NASWI_Aggregated_Noisemap - Aggregate_ContourLine_Lines - VALID/_FlightTracks_Lines.shp')
flighttracks = st_set_crs(flighttracks, crs)
runways = st_read('data/flight_ops/modeling/baseops/Aggregated/DNL/NASWI_Aggregated_Noisemap - Aggregate_ContourLine_Lines - VALID/_Runways_Lines.shp')
runways = st_set_crs(runways, crs)

mapview(contours_Ldn, zcol='Level') +
  # mapview(population_bg, col.regions=list('white')) +
  mapview(flighttracks, lwd = 0.5) + mapview(runways, lwd = 5) +
  mapview(sites)

