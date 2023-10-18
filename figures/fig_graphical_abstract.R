# Layered map stack with
# - Acoustic monitoring locations (dots)
# - Aircraft operations data (flight racks)
# - Simulated noise contour (Ldn)
# - Population density map (heatmap)
# +
#   Exposure-response functions and thresholds
# +
#   Population health estimates

# Various figures
source('figures/fig_map_global.R')
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

pop_areas_stack = stack(glue('{input_path}/pop_areas_stack.grd'))
mapview(pop_areas_stack[['Island_county']])

############

rotate_data <- function(data, x_add = 0, y_add = 0) {
  
  shear_matrix <- function(){ matrix(c(2, 1.2, 0, 1), 2, 2) }
  
  rotate_matrix <- function(x){ 
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
  }
  data %>% 
    dplyr::mutate(
      geometry = .$geometry * shear_matrix() * rotate_matrix(pi/20) + c(x_add, y_add)
    )
}

rotate_data_geom <- function(data, x_add = 0, y_add = 0) {
  shear_matrix <- function(){ matrix(c(2, 1.2, 0, 1), 2, 2) }
  
  rotate_matrix <- function(x) { 
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
  }
  data %>% 
    dplyr::mutate(
      geom = .$geom * shear_matrix() * rotate_matrix(pi/20) + c(x_add, y_add)
    )
}

library(ggplot2)
library(patchwork)

contours = get_contours_Ldn(threshold = threshold_adverse_health_effects_Lden)
contours = st_transform(contours, crs = st_crs(naswi_land))

bbox = st_bbox(contours)
layer_land = st_crop(st_make_valid(naswi_land), bbox)
layer_sites = st_crop(st_make_valid(sites), bbox)
layer_tracks = st_crop(st_make_valid(flight_tracks), bbox)
layer_noise = st_crop(st_make_valid(contours), bbox)

# layer_pop = st_crop(st_make_valid(pop_areas_stack[['Island_county']]), bbox)

layer_land = st_as_sf(layer_land)
st_geometry(layer_land) = 'geometry'
layer_sites = st_as_sf(layer_sites)
st_geometry(layer_sites) = 'geometry'
layer_tracks = st_as_sf(layer_tracks)
st_geometry(layer_tracks) = 'geometry'
layer_noise = st_as_sf(layer_noise)
st_geometry(layer_noise) = 'geometry'

pop = get_acs(geography = 'block group', variables = 'B01003_001', year = 2021, state = 'WA', geometry = T)
pop = st_transform(pop, crs = st_crs(naswi_land))
pop = st_crop(pop, bbox)

island = pop_areas_stack[['Island_county']]
island.aggregate = raster::aggregate(island, fact = 10)
island_sf = st_as_sf(as(island.aggregate, 'SpatialPolygonsDataFrame'))
island_sf = st_transform(island_sf, crs = st_crs(naswi_land))
island_sf = st_crop(island_sf, bbox)

jefferson = pop_areas_stack[['Jefferson_county']]
jefferson.aggregate = raster::aggregate(jefferson, fact = 10)
jefferson_sf = st_as_sf(as(jefferson.aggregate, 'SpatialPolygonsDataFrame'))
jefferson_sf = st_transform(jefferson_sf, crs = st_crs(naswi_land))
jefferson_sf = st_crop(jefferson_sf, bbox)

sanjuan = pop_areas_stack[['San.Juan_county']]
sanjuan.aggregate = raster::aggregate(sanjuan, fact = 10)
sanjuan_sf = st_as_sf(as(sanjuan.aggregate, 'SpatialPolygonsDataFrame'))
sanjuan_sf = st_transform(sanjuan_sf, crs = st_crs(naswi_land))
sanjuan_sf = st_crop(sanjuan_sf, bbox)

skagit = pop_areas_stack[['Skagit_county']]
skagit.aggregate = raster::aggregate(skagit, fact = 10)
skagit_sf = st_as_sf(as(skagit.aggregate, 'SpatialPolygonsDataFrame'))
skagit_sf = st_transform(skagit_sf, crs = st_crs(naswi_land))
skagit_sf = st_crop(skagit_sf, bbox)

snohomish = pop_areas_stack[['Snohomish_county']]
snohomish.aggregate = raster::aggregate(snohomish, fact = 10)
snohomish_sf = st_as_sf(as(snohomish.aggregate, 'SpatialPolygonsDataFrame'))
snohomish_sf = st_transform(snohomish_sf, crs = st_crs(naswi_land))
snohomish_sf = st_crop(snohomish_sf, bbox)

library(ggnewscale)

coords = st_sfc(
  st_polygon(list(
    rbind(
      c(bbox[1], bbox[2]),
      c(bbox[3], bbox[2]),
      c(bbox[3], bbox[4]),
      c(bbox[1], bbox[4]),
      c(bbox[1], bbox[2])
    )
  ))
)

# Create a simple feature collection with 1 feature and 0 fields
outline = st_sf(geometry = coords)
st_crs(outline) = st_crs(naswi_land)
mapview(outline)

spacer = seq(from = 0.0, to = 1.25, length.out = 4)

p = ggplot() +
  geom_sf(data = outline %>% rotate_data(y_add = spacer[1]), fill=alpha('white', 0.75), color=alpha('black', 0.5)) +
  geom_sf(data = layer_land %>% rotate_data(y_add = spacer[1]), fill='white') +
  geom_sf(data = layer_sites %>% rotate_data(y_add = spacer[1]), aes(col = Org), size = 1, show.legend = F) +
  scale_color_manual(values = c('#619CFF','#F8766D','#00BA38')) +
  
  new_scale_colour() +
  geom_sf(data = outline %>% rotate_data(y_add = spacer[2]), fill=alpha('white', 0.75), color=alpha('black', 0.5)) +
  geom_sf(data = layer_tracks %>% rotate_data(y_add = spacer[2]), color=alpha('#005eff', 0.1), lwd=0.9) +
  
  geom_sf(data = outline %>% rotate_data(y_add = spacer[3]), fill=alpha('white', 0.75), color=alpha('black', 0.5)) +
  geom_sf(data = layer_noise %>% rotate_data(y_add = spacer[3]), aes(fill=Level, color=Level), show.legend = F) +
  scale_fill_viridis(option = 'plasma', alpha = 0.75) +
  scale_color_viridis(option = 'plasma', alpha = 0.75) +
  
  new_scale_fill() +
  geom_sf(data = outline %>% rotate_data(y_add = spacer[4]), fill=alpha('white', 0.75), color=alpha('black', 0.5)) +
  geom_sf(data = island_sf %>% rotate_data(y_add = spacer[4]), aes(fill=Island_county), linewidth=0, show.legend = F) +
  geom_sf(data = jefferson_sf %>% rotate_data(y_add = spacer[4]), aes(fill=Jefferson_county), linewidth=0, show.legend = F) +
  geom_sf(data = sanjuan_sf %>% rotate_data(y_add = spacer[4]), aes(fill=San.Juan_county), linewidth=0, show.legend = F) +
  geom_sf(data = skagit_sf %>% rotate_data(y_add = spacer[4]), aes(fill=Skagit_county), linewidth=0, show.legend = F) +
  geom_sf(data = snohomish_sf %>% rotate_data(y_add = spacer[4]), aes(fill=Snohomish_county), linewidth=0, show.legend = F) +
  # geom_sf(data = pop %>% rotate_data(y_add = spacer[4]), aes(fill=estimate), show.legend = F) +
  scale_fill_viridis(alpha = 1.0) +
  
  ggsn::blank(); p

ggsave(
  plot = p,
  filename = glue('{output_path}/graphical_abstract.png'),
  width = 15, height = 15, dpi = 300
)
