source('global.R')
source('analysis/spatial_exposure.R')

########################################################################################################
## Noise complaint reports

data_reports = get_data_complaint_reports()
data_reports = data_reports[!is.na(data_reports$Longitude) & !is.na(data_reports$Longitude),]
sf_reports = st_as_sf(data_reports,
                      coords = c('Longitude', 'Latitude'),
                      crs = crs, agr = 'constant')
sf_reports$Longitude = st_coordinates(sf_reports$geometry)[,'X']
sf_reports$Latitude  = st_coordinates(sf_reports$geometry)[,'Y']

map_reports = ggplot() +
  geom_sf(data = population_bg) + # aes(fill = value)
  geom_sf(data = sf_reports, size = 1, shape = 19, color = 'red', alpha=0.1)
print(map_reports)

mapview(sf_reports, zcol='Character')
mapview(sf_reports) + mapview(exposure_Ldn, zcol='Level')

# TODO: cull reports over water
# TODO: reports for only the Navy monitoring weeks