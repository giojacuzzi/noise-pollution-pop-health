library(ggsn)
library(tigris)
options(tigris_use_cache = T)

source('global.R')

output_path = paste0(here::here(), '/figures/_output')

# Plot maps

bounds_x = c(-122.86, -122.33) # [min, max]
bounds_y = c(48.09, 48.47)
bounds = data.frame(
  x = c(bounds_x[1], bounds_x[1], bounds_x[2], bounds_x[2]),
  y = c(bounds_y[1], bounds_y[2], bounds_y[2], bounds_y[1])
)

labels_large = data.frame(
  Name      = c('Oak Harbor', 'Coupeville', 'Port Townsend', 'Anacortes', 'Camano Island', 'La Conner'),
  Lat  = c(48.297324,   48.213824,   48.125762,   48.502063, 48.2415588,   48.389814),
  Lon = c(-122.659911, -122.675396, -122.799917, -122.620023, -122.490305, -122.470759)
)

labels_medium = data.frame(
  Name = c('Swinomish\nReservation', 'Samish TDSA'),
  Lat  = c(48.424, 48.452),
  Lon  = c(-122.529, -122.593)
)

labels_small = data.frame(
  Name = c('Lopez Island'),
  Lat  = c(48.438),
  Lon  = c(-122.835)
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
sites$Org = factor(sites$Org, levels=c('NAVY','JGL','NPS'))
# sites = na.omit(sites)
# sites = sites[sites$ID %in% unique(get_data_metrics()[,'ID']), ]
sites$Longitude = st_coordinates(sites$geometry)[,'X']
sites$Latitude  = st_coordinates(sites$geometry)[,'Y']

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
naswi_sites$LocationOffset = naswi_sites$Location
naswi_sites[!(naswi_sites$LocationOffset %in% c(13,15,19)), 'LocationOffset'] = ''

flight_tracks_union = st_union(flight_tracks)

airfields = data.frame(
  Airfield = c('Ault Field', 'OLF Coupeville'),
  Lat = c(48.350, 48.190),
  Lon = c(-122.652,-122.629)
)

waterbodies = data.frame(
  Name = c('SALISH SEA'),
  Lat = c(48.28061078205637),
  Lon = c(-122.80557167382709)
)

native_areas = get_acs(geography = 'american indian area/alaska native area/hawaiian home land', variables = 'B01003_001', year = 2021, geometry = T)
native_areas = native_areas[native_areas$GEOID %in% c(8750,4075),] # Samish, Swinomish
native_areas = st_intersection(st_transform(native_areas, st_crs(naswi_land)), st_make_valid(naswi_land))

naswi_map = ggplot() +
  geom_sf(data = naswi_land, fill = 'white') +
  geom_sf(data = native_areas$geometry, fill=c('#11FF0044','#FFFF0044'), color='#FFFFFF00') + # #11FFFF44
  geom_sf(data = wa_roads$geometry, color='#DDDDDD', lwd=0.25) +
  geom_sf(data = native_areas$geometry, fill='#FFFFFF00', linetype='dotted') +
  geom_sf(data = wa_military$geometry, fill='#FF000044', color='#444444', lwd=0.4) +
  geom_sf(data = naswi_land, fill=NA) +
  geom_sf(data = runways, lwd=1, color='#555555') +
  geom_text(data = waterbodies, aes(x = Lon, y = Lat, label = Name), size = 5.5, col = '#b6e3fc', fontface = 'bold', angle=67) +
  geom_sf(data = flight_tracks_union, lwd=0.4, color=alpha('#005eff', 0.1),) + # alpha('#005eff', 0.1)
  geom_sf(data = naswi_sites, size = 5, aes(col = Org)) + # aes(shape = Org, col = Org)
  geom_sf_text(data = naswi_sites[naswi_sites$LocationOffset == '',], aes(label=Location), size=2.8, color='white') +
  geom_text(data = labels_large, aes(x = Lon, y = Lat, label = Name), size = 3.5, col = '#222222') +
  geom_text(data = labels_medium, aes(x = Lon, y = Lat, label = Name), size = 3.0, col = '#222222') +
  geom_text(data = labels_small, aes(x = Lon, y = Lat, label = Name), size = 2.5, col = '#222222') +
  scale_color_manual(name='Monitoring Locations', values = c('#619CFF','#F8766D','#00BA38')) +
  geom_text_repel(data = naswi_sites[naswi_sites$LocationOffset != '', ],
                  aes(x = Longitude, y = Latitude, label = LocationOffset),
                  size = 3, col = '#222222', max.overlaps = 5, # fontface = '',
                  force = 10,
                  nudge_x = c(0.03,0.025,0.0),
                  nudge_y = c(-0.008,-0.018,-0.028)) +
  geom_label_repel(data = airfields,
                  aes(x = Lon, y = Lat, label = Airfield),
                  size = 4, col = '#222222', max.overlaps = 5, fontface = 'bold',
                  force = 10,
                  nudge_x = c(0.08,0.08),
                  nudge_y = c(-0.025,0.02)) +
  coord_sf(xlim = bounds_x, ylim = bounds_y, expand = F) +
  # labs(x='', y='') +
  # north(naswi_sites, symbol = 3, scale = 0.11, location='bottomleft', anchor = c(x=-122.85677080867502, y=48.17642123618527)) +
  # scalebar(naswi_land, dist = 2, dist_unit = 'km', transform = T, model = 'WGS84', st.bottom = F, st.size=4, height = 0.02) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        # legend.position="none",
        panel.background=element_rect(fill = '#EAF7FE', color = NA),
        panel.border=element_rect(colour = '#222222', fill=NA, linewidth=0.75),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        # plot.background=element_blank(),
        plot.margin = margin(0, 0, 0, 0, 'pt')
        ); #naswi_map
# ggsave(naswi_map + theme(text=element_text(size=20), axis.text=element_text(size=12), plot.margin = margin(1,1,1,1, 'cm')), file=glue('{output_path}/monitoring_sites.png'))

# library(geodata)
# states = st_as_sf(geodata::gadm("United States", level = 1, path = "."))
# provinces = st_as_sf(geodata::gadm("Canada", level = 1, path = "."))

library(raster)
us = getData("GADM",country="USA",level=1)
canada = getData("GADM",country="CAN",level=1)
us.states = us[us$NAME_1 %in% c('Washington', 'Oregon', 'Idaho'),]
ca.provinces = canada[canada$NAME_1 %in% c('British Columbia'),]

us.bbox <- bbox(us.states)
ca.bbox <- bbox(ca.provinces)
xlim <- c(min(us.bbox[1,1],ca.bbox[1,1]),max(us.bbox[1,2],ca.bbox[1,2]))
ylim <- c(min(us.bbox[2,1],ca.bbox[2,1]),max(us.bbox[2,2],ca.bbox[2,2]))
states = st_as_sf(us.states)
provinces = st_as_sf(ca.provinces)

naswi_areas = data.frame(
  Name = c('NASWI', 'Olympic MOA'),
  Lat = c(48.311188428288645, 47.62867291138792), 
  Lon = c(-122.64312257481372,-124.086078720366)
)

state_label = data.frame(
  Name = c('WASHINGTON'),
  Lat = c(47.49471443497048),
  Lon = c(-119.50519234625058)
)

wa_map = ggplot() +
  geom_sf(data = states, fill = '#EEEEEE') +
  geom_sf(data = provinces, fill = '#EEEEEE') +
  geom_sf(data = wa_counties_cb, fill='white', color='#DDDDDD') +
  geom_sf(data = states, fill = NA) +
  geom_polygon(data = bounds, aes(x, y, group = 1), fill=NA, color = 'red') +
  geom_polygon(data = moa_bounds, aes(x=Long, y=Lat, group = 1), fill=NA, color = 'red', linetype='dashed') +
  # geom_sf(data = native_areas$geometry, fill=c('#11FF0044','#11FFFF44'), color='#FFFFFF00') +
  geom_sf(data = wa_military$geometry, fill='#FF000044', color='#444444', lwd=0.2) +
  # geom_sf(data = flight_tracks, lwd=0.5, color=alpha('blue', 0.1),) +
  # geom_sf(data = moa_tracks, lwd=0.1, color=alpha('blue', 0.5)) +
  geom_sf(data = sites[sites$ID %in% c('99_HOH'), ], size = 5, aes(shape = Org), color='#619CFF') +
  geom_text(data = state_label, aes(x = Lon, y = Lat, label = Name), size = 4, col = '#AAAAAA', fontface = 'bold') +
  geom_sf_text(data = sites[sites$ID %in% c('99_HOH'), ], aes(label=Location), size=3, color='white') +
  geom_label_repel(data = naswi_areas,
                   aes(x = Lon, y = Lat, label = Name),
                   size = 4, col = '#222222', max.overlaps = 5, fontface = 'bold',
                   force = 10,
                   nudge_x = c(2.2,2.7),
                   nudge_y = c(0.1,-1.0)) +
  coord_sf(xlim = c(-125, -116.8), ylim = c(45.5,49.2), expand = F) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_rect(fill = '#EAF7FE', color = NA),
        panel.border=element_rect(colour = '#222222', fill=NA, linewidth=0.75),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        plot.margin = margin(0, 0, 0, 0, 'pt')
        ); #print(wa_map)

# print(wa_map, vp = viewport(0.79, 0.128, width = 0.36, height = 0.36))

library(patchwork)
map_combined = naswi_map + theme(legend.position='none') +
  inset_element(wa_map, left=0.6, right=0.99, bottom=-0.24, top=0.5, align_to = 'full'); map_combined

ggsave(
  plot = map_combined,
  filename = glue('{output_path}/map.png')
)

ggsave(
  plot = cowplot::get_legend(naswi_map),
  filename = glue('{output_path}/map_legend.png')
)
