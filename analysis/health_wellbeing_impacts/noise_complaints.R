source('global.R')
source('analysis/population_exposure.R')

########################################################################################################
## Noise complaint reports

# TODO: cull reports over water
# TODO: reports for only the Navy monitoring weeks
get_sf_reports = function() {
  data_reports = get_data_complaint_reports()
  data_reports = data_reports[!is.na(data_reports$Longitude) & !is.na(data_reports$Longitude),]
  sf_reports = st_as_sf(data_reports,
                        coords = c('Longitude', 'Latitude'),
                        crs = crs, agr = 'constant')
  sf_reports$Longitude = st_coordinates(sf_reports$geometry)[,'X']
  sf_reports$Latitude  = st_coordinates(sf_reports$geometry)[,'Y']
  return(sf_reports)
}

# map_reports = ggplot() +
#   geom_sf(data = population_bg) + # aes(fill = value)
#   geom_sf(data = sf_reports, size = 1, shape = 19, color = 'red', alpha=0.1)
# print(map_reports)

if (!exists('sf_reports')) sf_reports = get_sf_reports

mapview(sf_reports, zcol='Character')
mapview(sf_reports) + mapview(exposure_Ldn, zcol='Level')
mapview(sf_reports) + mapview(exposure_Ldn[as.numeric(exposure_Ldn$Level)>=45,], zcol='Level')
