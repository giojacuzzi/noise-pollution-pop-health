library(ggsn)
library(tigris)
options(tigris_use_cache = T)

source('global.R')

# Plot maps

bounds_x = c(-123.0, -122.32) # [min, max]
bounds_y = c(48.05, 48.55)
bounds = data.frame(
  x = c(bounds_x[1], bounds_x[1], bounds_x[2], bounds_x[2]),
  y = c(bounds_y[1], bounds_y[2], bounds_y[2], bounds_y[1])
)

# wa_state = states(cb=T) %>% filter(NAME=='Washington')

wa_counties_cb = counties(state = 'WA', cb = T)
wa_counties_cb = st_transform(wa_counties_cb, 'WGS84')

# wa_water = c()
# for (c in wa_counties_cb$NAME) {
#   wa_water = rbind(wa_water, area_water(state = 'WA', county = c))
# }
# wa_water = st_union(wa_water)
flight_tracks = st_read('data/gis/NASWI/FlightTracks_Lines.shp', quiet = T)
flight_tracks = st_set_crs(flight_tracks, 'WGS84')

moa_tracks = st_read('data/gis/NASWI/MOA_MilitaryTrainingRoutes_MilitaryTrainingRoutes_Lines.shp', quiet = T)
moa_tracks = st_set_crs(moa_tracks, 'WGS84')

moa_bounds = read.csv('data/gis/NASWI/moa_bounds.csv')
moa_bounds$Long = -moa_bounds$Long
# names(moa_bounds) = c('x','y')

sites = st_as_sf(get_data_sites(), coords = c('Longitude', 'Latitude'), crs = 'WGS84', agr = 'constant')
# sites = na.omit(sites)
# sites = sites[sites$ID %in% unique(get_data_metrics()[,'ID']), ]
sites$Longitude = st_coordinates(sites$geometry)[,'X']
sites$Latitude  = st_coordinates(sites$geometry)[,'Y']

wa_map = ggplot() +
  geom_sf(data = wa_counties_cb, color='white') +
  geom_polygon(data = bounds, aes(x, y, group = 1), fill=NA, color = 'red') +
  geom_polygon(data = moa_bounds, aes(x=Long, y=Lat, group = 1), fill=NA, color = 'red', linetype='dotted') +
  geom_sf(data = flight_tracks, lwd=0.5, color=alpha('blue', 0.1),) +
  geom_sf(data = moa_tracks, lwd=0.1, color=alpha('blue', 0.5)) +
  geom_sf(data = sites[sites$ID %in% c('99_HOH'), ], size = 2, aes(shape = Org, col = Org)) +
  labs(x='', y='') +
  north(wa_counties_cb, location = 'topleft', symbol = 3, scale = 0.075) +
  # scalebar(wa_counties_cb, dist = 4, dist_unit = 'km', transform = T, model = 'WGS84', st.bottom = F, st.size=4, height = 0.01) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        # panel.border=element_rect(colour = '#111111', fill=NA, linewidth=1),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        plot.margin = margin(0, 0, 0, 0, 'pt')); print(wa_map)


###

locations = data.frame(
  Name      = c('Oak Harbor', 'Coupeville', 'Port Townsend', 'Anacortes', 'Lopez Island', 'Camano Island', 'Mt Vernon', 'La Conner'),
  Lat  = c(48.297324, 48.213824, 48.121762, 48.502063, 48.478226, 48.215588, 48.422988, 48.388814),
  Lon = c(-122.659911, -122.682396, -122.777917, -122.620023, -122.8991, -122.489305, -122.354446, -122.497759)
)

wa_roads = c()
for (c in c('Island', 'Jefferson', 'San Juan', 'Skagit', 'Snohomish', 'Clallam')) {
  wa_roads = rbind(wa_roads, roads(state = 'WA', county = c))
}

wa_military = st_crop(military(year = 2021), xmin = bounds_x[1], ymin = bounds_y[1], xmax = bounds_x[2], ymax = bounds_y[2])
wa_military = wa_military[wa_military$FULLNAME %in% c('Naval Air Station Whidbey Island', 'Naval Outlying Field Coupeville'), ]

runways = st_read('data/gis/NASWI/NASWI_Runways_Lines.shp', quiet = T)
runways = st_set_crs(runways, 'WGS84')

naswi_water = c()
for (c in c('Island', 'Jefferson', 'San Juan', 'Skagit', 'Snohomish', 'Clallam')) {
  naswi_water = rbind(naswi_water, st_transform(area_water(state = 'WA', county = c), 'WGS84'))
}
naswi_water = st_union(naswi_water)

naswi_land = st_union(wa_counties_cb)
naswi_land = st_difference(naswi_land, naswi_water)

naswi_sites = na.omit(sites)
naswi_sites = sites[sites$ID %in% unique(get_data_metrics()[,'ID']), ]

inset = ggplot() +
  geom_sf(data = naswi_land) +
  geom_sf(data = wa_roads$geometry, color='lightgray', lwd=0.3) +
  geom_sf(data = naswi_land, fill=NA) +
  geom_sf(data = wa_military$geometry, fill='#FF000044') +
  geom_sf(data = runways, lwd=1, color='darkgray') +
  geom_sf(data = naswi_sites, size = 2, aes(shape = Org, col = Org)) +
  geom_text_repel(data = naswi_sites,
                  aes(x = Longitude, y = Latitude, label = ID),
                  size = 2.5, col = 'black', fontface = 'bold', max.overlaps = 30,
                  nudge_x = c(),
                  nudge_y = c()) +
  coord_sf(xlim = bounds_x, ylim = bounds_y, expand = F) +
  labs(x='', y='') +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_rect(fill = 'white', color = NA),
        panel.border=element_rect(colour = '#111111', fill=NA, linewidth=1),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        # plot.background=element_blank(),
        plot.margin = margin(0, 0, 0, 0, 'pt'))

print(inset, vp = viewport(0.59, 0.49, width = 0.5, height = 0.5))

      