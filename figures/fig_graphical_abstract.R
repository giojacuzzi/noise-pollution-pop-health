# Various figures
source('figures/fig_global.R')
source('simulation/contours.R')

source('metrics/metrics.R')
source('metrics/thresholds.R')
source('metrics/exposure_response_functions.R')

input_path = paste0(here::here(), '/analysis/population_noise_exposure/_output/noise_exposure')
pop_exposure_stack = stack(glue('{input_path}/pop_exposure_stack.grd'))

# Study land area
mapview(naswi_land)

# Acoustic monitoring locations (dots)
sites = st_as_sf(get_data_sites(), coords = c('Longitude', 'Latitude'), crs = 'WGS84', agr = 'constant')
sites$Longitude = st_coordinates(sites$geometry)[,'X']
sites$Latitude  = st_coordinates(sites$geometry)[,'Y']
mapview(sites)

# Aircraft operations data (flight tracks)
flight_tracks = st_read('data/gis/NASWI/FlightTracks_Lines.shp', quiet = T)
flight_tracks = st_set_crs(flight_tracks, 'WGS84')
mapview(flight_tracks)

# Simulated noise contour (Ldn)
mapview(pop_exposure_stack[['Ldn']])

# Population density map (heatmap)
mapview(pop_exposure_stack[['Exposed.Population']])