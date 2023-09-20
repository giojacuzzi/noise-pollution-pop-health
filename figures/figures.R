# Various figures
library(patchwork)
library(ggsn)
library(OpenStreetMap)
library(osmdata)
library(tigris)
options(tigris_use_cache = T)

source('global.R')
source('simulation/contours.R')

source('metrics/metrics.R')
source('metrics/thresholds.R')
source('metrics/exposure_response_functions.R')

input_path = paste0(here::here(), '/analysis/population_noise_exposure/_output/noise_exposure')
output_path = paste0(here::here(), '/figures/_output')
pop_exposure_stack = stack(glue('{input_path}/pop_exposure_stack.grd'))

# Level scale to unify colors across plots
level_scale = seq(40,90,5)

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
exposure_levels_Ldn = exposure_levels_Ldn %>%
  mutate(Level = (Level) %/% 5 * 5) %>%
  group_by(Level) %>%
  summarise(Population = sum(Population))

exposure_levels_Ldn$Level = factor(exposure_levels_Ldn$Level, levels=level_scale)
exposure_levels_Ldn = exposure_levels_Ldn[exposure_levels_Ldn$Population != 0, ]
p_pop_exposed_per_5dB = ggplot(exposure_levels_Ldn, aes(x=Level, y=Population, fill=Level)) +
  geom_bar(position='dodge', stat='identity') +
  scale_fill_viridis_d(option='plasma', drop = F, name=expression(L[dn]~'dB('*A*')')) +
  ggtitle(expression('Estimated population exposed per 5 dB'~L[dn])) + xlab(expression(L[dn])) +
  ylab('Population') +
  geom_text(aes(label=round(Population)), position=position_dodge(width=0.9), vjust=-0.25, size=7); p_pop_exposed_per_5dB
ggsave(p_pop_exposed_per_5dB + theme(text=element_text(size=22), plot.margin = margin(1,1,1,1, 'cm')), file=glue('{output_path}/pop_exposed_per_5dB.png'), width=10, height=9)

# Plot maps

bounds_x = c(-123.0, -122.32) # [min, max]
bounds_y = c(48.05, 48.55)
bounds = data.frame(
  x = c(bounds_x[1], bounds_x[1], bounds_x[2], bounds_x[2]),
  y = c(bounds_y[1], bounds_y[2], bounds_y[2], bounds_y[1])
)

wa_counties_cb = counties(state = 'WA', cb = T)

wa_roads = c()
for (c in c('Island', 'Jefferson', 'San Juan', 'Skagit', 'Snohomish', 'Clallam')) {
  wa_roads = rbind(wa_roads, roads(state = 'WA', county = c))
}

wa_water = c()
for (c in c('Island', 'Jefferson', 'San Juan', 'Skagit', 'Snohomish', 'Clallam')) {
  wa_water = rbind(wa_water, area_water(state = 'WA', county = c))
}
wa_water = st_union(wa_water)

wa_land = st_union(wa_counties_cb)
wa_land = st_difference(wa_land, wa_water)

# study_area = st_crop(wa_counties_cb, xmin = bounds_x[1], ymin = bounds_y[1], xmax = bounds_x[2], ymax = bounds_y[2])

locations = data.frame(
  Name      = c('Oak Harbor', 'Coupeville', 'Port Townsend', 'Anacortes', 'Lopez Island', 'Camano Island', 'Mt Vernon', 'La Conner'),
  Lat  = c(48.297324, 48.213824, 48.121762, 48.502063, 48.478226, 48.215588, 48.422988, 48.388814),
  Lon = c(-122.659911, -122.682396, -122.777917, -122.620023, -122.8991, -122.489305, -122.354446, -122.497759)
)

wa_map = ggplot() +
  geom_sf(data = wa_counties_cb) +
  geom_polygon(data = bounds, aes(x, y, group = 1), fill=NA, color = 'red') +
  labs(x='', y='') +
  theme_minimal(); print(wa_map)

wa_military = st_crop(military(year = 2021), xmin = bounds_x[1], ymin = bounds_y[1], xmax = bounds_x[2], ymax = bounds_y[2])
wa_military = wa_military[wa_military$FULLNAME %in% c('Naval Air Station Whidbey Island', 'Naval Outlying Field Coupeville'), ]

runways = st_read('data/gis/NASWI/NASWI_Runways_Lines.shp', quiet = T)
runways = st_set_crs(runways, crs)

sites = st_as_sf(get_data_sites(), coords = c('Longitude', 'Latitude'), crs = crs, agr = 'constant')
sites = na.omit(sites)
sites = sites[sites$ID %in% unique(get_data_metrics()[,'ID']), ]
sites$Longitude = st_coordinates(sites$geometry)[,'X']
sites$Latitude  = st_coordinates(sites$geometry)[,'Y']

wa_landmarks = landmarks(state = 'WA', type = 'area')
wa_landmarks = wa_landmarks %>% drop_na(FULLNAME)

wa_parks = wa_landmarks[(str_detect(wa_landmarks$FULLNAME, 'Park') | str_detect(wa_landmarks$FULLNAME, 'Pk')),]

ggplot() +
  geom_sf(data = wa_land) +
  # geom_sf(data = wa_parks, fill='green') +
  geom_sf(data = wa_roads$geometry, color='lightgray', lwd=0.3) +
  geom_sf(data = wa_land, fill=NA) +
  # geom_sf(data = wa_counties_cb, fill=NA) +
  # geom_sf(data = wa_water, fill='blue') +
  geom_sf(data = wa_military$geometry, fill='#FF000044') +
  geom_sf(data = runways, lwd=1, color='darkgray') +
  geom_sf(data = sites, size = 2, aes(shape = Org, col = Org)) +
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
  get_contours_Ldn(), #(threshold = lden_impact_threshold),
  get_contours_Lnight(), #(threshold = lnight_impact_threshold),
  get_contours_Leq24())#(threshold = HL_leq24_impact_threshold))

# contours[[1]]$Level = factor(contours[[1]]$Level, levels=level_scale)
# contours[[2]]$Level = factor(contours[[2]]$Level, levels=level_scale)
# contours[[3]]$Level = factor(contours[[3]]$Level, levels=level_scale)

# Combine contours in 5 dB increments
contours_5dB = contours
for (i in 1:3) {
  c = st_make_valid(contours[[i]])
  c_5dB = NULL
  for (l in seq(from=min(c$Level), to=max(c$Level), by=5)) {
    message(l)
    u = st_union(c[c$Level >= l & c$Level < l + 5, ])
    u = st_make_valid(st_as_sf(u))
    u$Level = l
    if (is.null(c_5dB)) {
      c_5dB = u
    }
    c_5dB = c_5dB %>% bind_rows(u)
  }
  contours_5dB[[i]] = c_5dB
}

cols = data.frame( # shared color scale among all level metrics
  level=level_scale,
  color=viridis_pal(option='C')(length(level_scale))
)

plot_contours = function(contours, threshold, lims, title, units, primary = F) {
  below = contours[as.numeric(contours$Level)<(threshold) & as.numeric(contours$Level)>=(threshold-10),]
  contours = contours[as.numeric(contours$Level)>=threshold,]
  contours$Level = factor(contours$Level, levels=level_scale)
  
  p = ggplot() +
    geom_sf(data = wa_land) +
    geom_sf(data = wa_roads$geometry, color='lightgray', lwd=0.3) +
    geom_sf(data = wa_land, fill=NA) +
    geom_sf(data = runways, lwd=1, color='darkgray') +

    geom_sf(data = st_cast(below[1,], 'MULTILINESTRING'), color='black', lwd=0.3, alpha=0.5, linetype='dashed') +

    geom_sf(data = contours, aes(fill=Level, color=Level), lwd=0.3) +
    scale_fill_manual(values=alpha(cols[cols$level %in% lims, 'color'], 0.5), name = units, guide = guide_legend(reverse = T)) +

    scale_color_manual(values=alpha(cols[cols$level %in% lims, 'color'], 0.7), name = units, guide = guide_legend(reverse = T)) +

    coord_sf(xlim = bounds_x, ylim = bounds_y, expand = F) +
    labs(title = title, x='', y='') +
    geom_text(data = locations, aes(x = Lon, y = Lat, label = Name), size = 3, col = '#222222', fontface = 'bold') +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          # legend.position="none",
          panel.background=element_blank(),
          panel.border=element_rect(colour = '#111111', fill=NA, linewidth=1),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          plot.margin = margin(0, 0, 0, 0, 'pt'))

  # if (primary) {
  #   p = p + #north(contours, symbol = 3, scale = 0.075) +
  #     #scalebar(contours, dist = 4, dist_unit = 'km', transform = T, model = 'WGS84', st.bottom = F, st.size=4, height = 0.01) +
  #     geom_text(data = locations, aes(x = Lon, y = Lat, label = Name), size = 3, col = '#222222', fontface = 'bold')
  # }
  return(p)
}
pLdn = plot_contours(contours_5dB[[1]], threshold = lden_impact_threshold, lims = seq(45,90,5), title = 'Day-night average sound level', units = expression(L[dn]~'dB('*A*')'), primary = T); pLdn
pLnight = plot_contours(contours_5dB[[2]], threshold = lnight_impact_threshold, lims = seq(40,90,5), title = 'Night average sound level', units = expression(L[night]~'dB('*A*')'))
pLeq24 = plot_contours(contours_5dB[[3]], threshold = HL_leq24_impact_threshold, lims = seq(70,90,5), title = '24 hour equivalent continuous sound level', units = expression(L[eq24]~'dB('*A*')'))
ggsave(pLdn + theme(text=element_text(size=22), plot.margin = margin(5,5,5,5, 'pt')), file=glue('{output_path}/contours_Ldn.png'), width=12, height=10)
ggsave(pLnight + theme(text=element_text(size=22), plot.margin = margin(5,5,5,5, 'pt')), file=glue('{output_path}/contours_Lnight.png'), width=12, height=10)
ggsave(pLeq24 + theme(text=element_text(size=22), plot.margin = margin(5,5,5,5, 'pt')), file=glue('{output_path}/contours_Leq24.png'), width=12, height=10)
# ggsave((pLdn + (pLnight / pLeq24)) + theme(text=element_text(size=22)), file=glue('{output_path}/contours.jpg'), width=20, height=20)

map = openmap(c(lat=bounds_y[1], lon=bounds_x[1]), c(lat=bounds_y[2], lon=bounds_x[2]), minNumTiles=9,type='osm') # 'osm', 'osm-transport
map.latlon = openproj(map, projection = projection(contours_5dB[[1]]))

map_contours = function(contours, title) {
  autoplot.OpenStreetMap(map.latlon) +
    geom_sf(data = st_cast(contours, 'MULTILINESTRING'), mapping=aes(color=Level), inherit.aes = F) +
    scale_color_viridis_d(option='plasma', alpha=1, drop = F, name = 'dB(A)') +
    geom_sf(data = contours, mapping=aes(fill=Level), lwd=0, inherit.aes=F) +
    scale_fill_viridis_d(option='plasma', alpha=0.25, drop = F, name = 'dB(A)') +
    labs(title = title, x='', y='') +
    theme_minimal()
}
p_map_ldn = map_contours(contours_5dB[[1]], title = 'Day-night average level, Ldn'); p_map_ldn
ggsave(p_map_ldn + theme(text=element_text(size=20), axis.text=element_text(size=12), plot.margin = margin(1,1,1,1, 'cm')), file=glue('{output_path}/map_ldn.png'), width=10, height=9)
p_map_lnight = map_contours(contours_5dB[[2]], title = 'Nighttime average level, Lnight'); p_map_lnight
ggsave(p_map_lnight + theme(text=element_text(size=20), axis.text=element_text(size=12), plot.margin = margin(1,1,1,1, 'cm')), file=glue('{output_path}/map_lnight.png'), width=10, height=9)
p_map_leq24 = map_contours(contours_5dB[[3]], title = '24-hour average level, Leq24'); p_map_leq24
ggsave(p_map_leq24 + theme(text=element_text(size=20), axis.text=element_text(size=12), plot.margin = margin(1,1,1,1, 'cm')), file=glue('{output_path}/map_leq24.png'), width=10, height=9)

### Schools

schools = read.csv('analysis/_output/schools.csv')
mapview(schools, xcol='LON', ycol='LAT', crs=crs)

schools_sf = st_as_sf(schools, coords = c('LON', 'LAT'), crs = crs, agr = 'constant')

ggplot() +
  geom_sf(data = wa_land) +
  geom_sf(data = schools_sf, size = 2, aes(color = Level>=55)) +
  geom_text_repel(data = na.omit(schools),
                  aes(x = LON, y = LAT, label = NAME),
                  size = 2.5, col = 'black', fontface = 'bold', max.overlaps = 30,
                  nudge_x = c(),
                  nudge_y = c()) +
  coord_sf(xlim = c(-122.7, -122.66), ylim = c(48.2, 48.23), expand = F) +
  labs(x='', y='') +
  theme_bw()

ggplot() +
  geom_sf(data = wa_land) +
  # geom_sf(data = wa_roads$geometry, color='lightgray', lwd=0.3) +
  # geom_sf(data = wa_land, fill=NA) +

  geom_sf(data = schools_sf, size = 2, aes(color = Level>=55)) +
  geom_text_repel(data = na.omit(schools),
                  aes(x = LON, y = LAT, label = NAME),
                  size = 2.5, col = 'black', fontface = 'bold', max.overlaps = 30,
                  nudge_x = c(),
                  nudge_y = c()) +
  coord_sf(xlim = c(-122.7, -122.55), ylim = c(48.2, 48.35), expand = F) +
  labs(x='', y='') +
  theme_bw()

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

p_pop_exposure = ggplot() +
  geom_sf(data = wa_counties_cb, fill='#150421') + # '#150421', '#222222', '#2c1d37'
  
  geom_tile(data=pop_exposure_SpatialPixelsDataFrame, aes(x=x, y=y, fill=Exposed.Population), alpha=1) +
  scale_fill_viridis_c(option='viridis', alpha=1, limits = c(0, 12), name=bquote('Population per 30m'^2)) +
  
  geom_sf(data =  st_cast(contours[[1]], 'MULTILINESTRING'), aes(color=factor(Level)), lwd=0.25,  linetype = 'dashed') +
  scale_color_viridis_d(option='plasma', alpha=0.6, drop = F, name='Ldn dB(A)') +
  
  coord_sf(xlim = bounds_x, ylim = bounds_y) +
  labs(title='Population exposure', x='', y='') +
  theme_bw() +
  theme(legend.direction="horizontal") +
  theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_line(color = '#FFFFFF11', linetype = 'dotted')); p_pop_exposure
ggsave(p_pop_exposure + theme(text=element_text(size=20), axis.text=element_text(size=12), plot.margin = margin(1,1,1,1, 'cm')), file=glue('{output_path}/pop_exposure.png'), width=10, height=9)

ggplot() +
  geom_sf(data = wa_counties_cb, fill='#222222') +
  geom_sf(data = runways) +
  geom_tile(data=pop_exposure_SpatialPixelsDataFrame, aes(x=x, y=y, fill=Exposed.Population), alpha=1) +
  scale_fill_viridis_c(option='plasma') +
  coord_sf(xlim = bounds_x, ylim = bounds_y) +
  labs(x='', y='') +
  theme_bw()

coords = matrix(c(bounds_x[1], bounds_x[2], bounds_y[1], bounds_y[2]), byrow = T, nrow = 2, ncol = 2, dimnames = list(c('x','y'),c('min','max')))
water = coords %>% opq() %>% add_osm_feature(key = "natural", value = c("water")) %>% osmdata_sf()
buildings = coords %>% opq() %>% add_osm_feature(key = "building") %>% osmdata_sf()
roads = coords %>% opq() %>% add_osm_feature(key = 'highway') %>% osmdata_sf()
parks <- location %>% add_osm_feature(key = "leisure", value = c("park","nature_reserve","recreation_ground","golf_course","pitch","garden")) %>% osmdata_sf()

water$osm_multipolygons = st_set_crs(water$osm_multipolygons, crs)
ggplot() +
  geom_sf(data = wa_counties_cb) +
  # geom_sf(data = st_difference(st_union(wa_counties_cb), st_union(water$osm_multipolygons)), lwd=0.1) +
  # geom_sf(data = water$osm_multipolygons) +
  geom_sf(data = roads$osm_multipolygons, color = 'red') +
  coord_sf(xlim = bounds_x, ylim = bounds_y)

map = openmap(c(lat=bounds_y[1], lon=bounds_x[1]), c(lat=bounds_y[2], lon=bounds_x[2]), minNumTiles=9,type='osm-transport') # 'osm'
map.latlon = openproj(map, projection = projection(contours[[1]]))
autoplot.OpenStreetMap(map.latlon) +
  geom_sf(data = st_cast(contours[[1]], 'MULTILINESTRING'), mapping=aes(color=Level), inherit.aes = F) +
  scale_color_viridis_d(option='plasma', alpha=1, drop = F) +
  geom_tile(data=pop_exposure_SpatialPixelsDataFrame, aes(x=x, y=y, fill=Exposed.Population), alpha=1) +
  scale_fill_viridis_c(option='plasma')
