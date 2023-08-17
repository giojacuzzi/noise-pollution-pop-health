# Various figures
library(tigris)
options(tigris_use_cache = T)

source('global.R')
source('simulation/contours.R')

source('metrics/metrics.R')
source('metrics/health_metrics.R')
source('metrics/exposure_response_functions.R')

input_path = paste0(here::here(), '/analysis/preprocessing/_output')
output_path = paste0(here::here(), '/figures/_output')
pop_exposure_stack = stack(glue('{input_path}/pop_exposure_stack.grd'))

# Level scale to unify colors across plots
l = seq(40,90,5)

# Population exposure per 5 dB
exposure_levels_Ldn = data.frame()
for (dB in unique(na.omit(as.vector(pop_exposure_stack[['Ldn']])))) {
  message('Exposure ', dB, ' dB')
  exp_masked = mask(pop_exposure_stack[['Exposed.Population']],
                    clamp(pop_exposure_stack[['Ldn']], lower=dB, upper=dB, useValues=F))
  exposure_levels_Ldn = rbind(exposure_levels_Ldn, data.frame(
    Level=dB,
    Population=cellStats(exp_masked, 'sum')
  ))
}
exposure_levels_Ldn$Level = factor(exposure_levels_Ldn$Level, levels=l)
exposure_levels_Ldn = exposure_levels_Ldn[exposure_levels_Ldn$Population != 0, ]
p_pop_exposed_per_5dB = ggplot(exposure_levels_Ldn, aes(x=Level, y=Population, fill=Level)) +
  geom_bar(position='dodge', stat='identity') +
  scale_fill_viridis_d(option='plasma', drop = F) +
  ggtitle('Estimated population exposed per 5dB Ldn') + xlab('Ldn (dB)') +
  ylab('Estimated population') +
  geom_text(aes(label=round(Population)), position=position_dodge(width=0.9), vjust=-0.25); p_pop_exposed_per_5dB
ggsave(glue('{output_path}/pop_exposed_per_5dB.png'), p_pop_exposed_per_5dB)

# Plot maps

bounds_x = c(-123.0, -122.32) # [min, max]
bounds_y = c(48.05, 48.55)
bounds = data.frame(
  x = c(bounds_x[1], bounds_x[1], bounds_x[2], bounds_x[2]),
  y = c(bounds_y[1], bounds_y[2], bounds_y[2], bounds_y[1])
)

wa_counties_cb = counties(state = 'WA', cb = T)

# study_area = st_crop(wa_counties_cb, xmin = bounds_x[1], ymin = bounds_y[1], xmax = bounds_x[2], ymax = bounds_y[2])

wa_map = ggplot() +
  geom_sf(data = wa_counties_cb) +
  geom_polygon(data = bounds, aes(x, y, group = 1), fill=NA, color = 'red') +
  labs(x='', y='') +
  theme_bw(); print(wa_map)

wa_military = st_crop(military(year = 2021), xmin = bounds_x[1], ymin = bounds_y[1], xmax = bounds_x[2], ymax = bounds_y[2])
wa_military = wa_military[wa_military$FULLNAME %in% c('Naval Air Station Whidbey Island', 'Naval Outlying Field Coupeville'), ]

runways = st_read('data/gis/NASWI/NASWI_Runways_Lines.shp', quiet = T)
runways = st_set_crs(runways, crs)

sites = st_as_sf(get_data_sites(), coords = c('Longitude', 'Latitude'), crs = crs, agr = 'constant')
sites = na.omit(sites)
sites = sites[sites$ID %in% unique(get_data_metrics()[,'ID']), ]
sites$Longitude = st_coordinates(sites$geometry)[,'X']
sites$Latitude  = st_coordinates(sites$geometry)[,'Y']

ggplot() +
  geom_sf(data = wa_counties_cb) +
  geom_sf(data = wa_military$geometry, fill='#FF000044') +
  geom_sf(data = runways, lwd=1, color='darkgray') +
  geom_sf(data = sites, size = 2, aes(shape = Org), color = 'black') +
  geom_text_repel(data = sites,
                  aes(x = Longitude, y = Latitude, label = ID),
                  size = 2.5, col = 'black', fontface = 'bold', max.overlaps = 30,
                  nudge_x = c(),
                  nudge_y = c()) +
  coord_sf(xlim = bounds_x, ylim = bounds_y, expand = F) +
  labs(x='', y='') +
  theme_bw()

### Noise spatial extent

contours = list(
  get_contours_Ldn(threshold = lden_impact_threshold),
  get_contours_Lnight(threshold = lnight_impact_threshold),
  get_contours_Leq24(threshold = HL_leq24_impact_threshold))

contours[[1]]$Level = factor(contours[[1]]$Level, levels=l)
contours[[2]]$Level = factor(contours[[2]]$Level, levels=l)
contours[[3]]$Level = factor(contours[[3]]$Level, levels=l)

plot_contours = function(contours, title) {
  ggplot() +
    geom_sf(data = wa_counties_cb) +
    
    geom_sf(data = contours, aes(fill=Level), lwd=0) +
    scale_fill_viridis_d(option='plasma', alpha=0.5, drop = F) +

    geom_sf(data = st_cast(contours, 'MULTILINESTRING'), aes(color=Level)) +
    scale_color_viridis_d(option='plasma', alpha=1, drop = F) +
    
    coord_sf(xlim = bounds_x, ylim = bounds_y, expand = F) +
    labs(title = title, x='', y='') +
    theme_bw()
}
plot_contours(contours[[1]], title = 'Ldn')
plot_contours(contours[[2]], title = 'Lnight')
plot_contours(contours[[3]], title = 'Leq24')

### Exposed population

pop_exposure = pop_exposure_stack[['Exposed.Population']]
pop_exposure_proj = projectRaster(pop_exposure, crs = projection(wa_counties_cb))
pop_exposure_SpatialPixelsDataFrame = data.frame(as(pop_exposure_proj, "SpatialPixelsDataFrame"))

# pop_unexposed = list()
# for (c in names(pop_exposure_stack[[1:5]])) {
#   message(c)
#   cty = pop_exposure_stack[[c]]
#   cty_proj = projectRaster(cty, crs = projection(wa_counties_cb))
#   cty_SpatialPixelsDataFrame = data.frame(as(cty_proj, "SpatialPixelsDataFrame"))
#   pop_unexposed = append(pop_unexposed, cty_SpatialPixelsDataFrame)
# }

ggplot() +
  geom_sf(data = wa_counties_cb, fill='#150421') +
  
  geom_tile(data=pop_exposure_SpatialPixelsDataFrame, aes(x=x, y=y, fill=Exposed.Population), alpha=1) +
  scale_fill_viridis_c(option='viridis', alpha=1, limits = c(0, 12)) +
  
  # geom_tile(data=cty_island_SpatialPixelsDataFrame, aes(x=x, y=y, fill=County.Island), alpha=1) +
  # scale_fill_gradient(low = '#150421', high = 'white', limits = c(0, 12)) +
  
  # geom_sf(data = st_intersection(wa_counties_cb, contours[[1]]), aes(fill=Level), lwd=0) +
  # scale_fill_viridis_d(option='plasma', alpha=0.7, drop = F) +
  
  # geom_sf(data = runways, lwd=1, color='darkgray', alpha=0.2) +
  
  # geom_sf(data = st_intersection(wa_counties_cb, st_cast(contours[[1]], 'MULTILINESTRING')), aes(color=Level), lwd=0.25) +
  geom_sf(data =  st_cast(contours[[1]], 'MULTILINESTRING'), aes(color=Level), lwd=0.25) +
  scale_color_viridis_d(option='plasma', alpha=0.6, drop = F) +

  coord_sf(xlim = bounds_x, ylim = bounds_y) +
  labs(x='', y='') +
  theme_bw()

ggplot() +
  geom_sf(data = wa_counties_cb, fill='#222222') +
  geom_sf(data = runways) +
  geom_tile(data=pop_exposure_SpatialPixelsDataFrame, aes(x=x, y=y, fill=Exposed.Population), alpha=1) +
  scale_fill_viridis_c(option='plasma') +
  coord_sf(xlim = bounds_x, ylim = bounds_y) +
  labs(x='', y='') +
  theme_bw()

library(osmdata)

coords = matrix(c(bounds_x[1], bounds_x[2], bounds_y[1], bounds_y[2]), byrow = T, nrow = 2, ncol = 2, dimnames = list(c('x','y'),c('min','max')))
water = coords %>% opq() %>% add_osm_feature(key = "natural", value = c("water")) %>% osmdata_sf()
parks <- location %>% add_osm_feature(key = "leisure", value = c("park","nature_reserve","recreation_ground","golf_course","pitch","garden")) %>% osmdata_sf()

water$osm_multipolygons = st_set_crs(water$osm_multipolygons, crs)
ggplot() +
  geom_sf(data = st_difference(st_union(wa_counties_cb), st_union(water$osm_multipolygons)), lwd=0.1) +
  # geom_sf(data = parks$osm_multipolygons) +
  coord_sf(xlim = bounds_x, ylim = bounds_y)

library(OpenStreetMap)
map = openmap(c(lat=bounds_y[1], lon=bounds_x[1]), c(lat=bounds_y[2], lon=bounds_x[2]), minNumTiles=9,type='osm-transport') # 'osm'
map.latlon = openproj(map, projection = projection(contours[[1]]))
autoplot.OpenStreetMap(map.latlon) +
  geom_sf(data = st_cast(contours[[1]], 'MULTILINESTRING'), mapping=aes(color=Level), inherit.aes = F) +
  scale_color_viridis_d(option='plasma', alpha=1, drop = F) +
  geom_tile(data=pop_exposure_SpatialPixelsDataFrame, aes(x=x, y=y, fill=Exposed.Population), alpha=1) +
  scale_fill_viridis_c(option='plasma')
