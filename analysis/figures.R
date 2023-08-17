library(raster)
library(sf)
library(glue)
library(gdalUtils)
library(rslurm)
library(fasterize)
library(mapview)
library(ggrepel)
mapviewOptions(mapview.maxpixels = 50000000)
options(tigris_use_cache = T)

source('global.R')
source('simulation/contours.R')

source('metrics/metrics.R')
source('metrics/health_metrics.R')
source('metrics/exposure_response_functions.R')

input_path = paste0(here::here(), '/analysis/preprocessing/_output')
output_path = paste0(here::here(), '/analysis/_output')
pop_exposure_stack = stack(glue('{input_path}/pop_exposure_stack.grd'))

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
p_pop_exposed_per_5dB = ggplot(exposure_levels_Ldn, aes(x=Level, y=Population, fill=Level)) +
  geom_bar(position='dodge', stat='identity') +
  scale_fill_viridis_c(option='inferno') +
  ggtitle('Estimated population exposed per 5dB Ldn') + xlab('Ldn (dB)') +
  ylab('Estimated population') +
  geom_text(aes(label=round(Population)), position=position_dodge(width=0.9), vjust=-0.25); p_pop_exposed_per_5dB
ggsave(glue('{output_path}/p_pop_exposed_per_5dB.png'), p_pop_exposed_per_5dB)

# Plot maps

bounds_x = c(-123.0, -122.3) # [min, max]
bounds_y = c(48.0, 48.6)
bounds = data.frame(
  x = c(bounds_x[1], bounds_x[1], bounds_x[2], bounds_x[2]),
  y = c(bounds_y[1], bounds_y[2], bounds_y[2], bounds_y[1])
)

wa_counties_cb = counties(state = 'WA', cb = T)
plot(wa_counties_cb$geometry)

# study_area = st_crop(wa_counties_cb, xmin = bounds_x[1], ymin = bounds_y[1], xmax = bounds_x[2], ymax = bounds_y[2])

wa_map = ggplot() +
  geom_sf(data = wa_counties_cb) +
  geom_polygon(data = bounds, aes(x, y, group = 1), fill=NA, color = 'red') +
  labs(x='', y='') +
  theme_bw(); print(wa_map)

wa_military = st_crop(military(year = 2021), xmin = bounds_x[1], ymin = bounds_y[1], xmax = bounds_x[2], ymax = bounds_y[2])
wa_military = wa_military[wa_military$FULLNAME != 'Naval Magazine Indian Island', ]

sites = st_as_sf(get_data_sites(), coords = c('Longitude', 'Latitude'), crs = crs, agr = 'constant')
sites = na.omit(sites)
sites = sites[sites$ID %in% unique(get_data_metrics()[,'ID']), ]
sites$Longitude = st_coordinates(sites$geometry)[,'X']
sites$Latitude  = st_coordinates(sites$geometry)[,'Y']

ggplot() +
  geom_sf(data = wa_counties_cb) +
  geom_sf(data = wa_military$geometry, fill='#FF000044') +
  geom_sf(data = sites, size = 2, aes(shape = Org), color = 'black') +
  geom_text_repel(data = sites,
                  aes(x = Longitude, y = Latitude, label = ID),
                  size = 2.5, col = 'black', fontface = 'bold', max.overlaps = 30,
                  nudge_x = c(),
                  nudge_y = c()) +
  coord_sf(xlim = bounds_x, ylim = bounds_y) +
  labs(x='', y='') +
  theme_bw()

contours_Ldn = get_contours_Ldn()
contours_Ldn = contours_Ldn[as.numeric(contours_Ldn$Level)>=lden_impact_threshold, ]
contours_Ldn$Level = factor(contours_Ldn$Level)

ggplot() +
  geom_sf(data = wa_counties_cb) +
  geom_sf(data = contours_Ldn, aes(fill=Level)) +
  scale_fill_viridis_d(option='plasma', alpha=0.5) +
  geom_sf(data = st_cast(contours_Ldn, 'MULTILINESTRING'), aes(color=Level)) +
  scale_color_viridis_d(option='plasma', alpha=1) +
  coord_sf(xlim = bounds_x, ylim = bounds_y) +
  labs(x='', y='') +
  theme_bw()

pop_exposure = pop_exposure_stack[['Exposed.Population']]
pop_exposure_proj = projectRaster(pop_exposure, crs = projection(wa_counties_cb))
pop_exposure_SpatialPixelsDataFrame = data.frame(as(pop_exposure_proj, "SpatialPixelsDataFrame"))

ggplot() +
  geom_sf(data = wa_counties_cb, fill='#222222') +
  geom_tile(data=pop_exposure_SpatialPixelsDataFrame, aes(x=x, y=y, fill=Exposed.Population), alpha=1) +
  scale_fill_viridis_c(option='plasma') +
  coord_sf(xlim = bounds_x, ylim = bounds_y) +
  labs(x='', y='') +
  theme_bw()

