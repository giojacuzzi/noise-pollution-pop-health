## Cognitive development in children and speech intelligibility

source('global.R')
source('metrics/metrics.R')
source('analysis/population_exposure.R')

data_sites   = get_data_sites()
data_metrics = get_data_metrics()
data_events = get_data_events()

sites_with_events = data_sites[data_sites$ID %in% unique(data_events$ID),]

########################################################################################################
# Children's learning and comprehension

# Ldn >= 55 dB poses risk of inhibited reading skills and oral comprehension in children (WHO, RANCH)
Ldn_gt55 = exposure_Ldn[as.numeric(exposure_Ldn$Level)>=55,]
Ldn_gt55_bounds = st_bbox(Ldn_gt55) #exposure_Ldn

# https://nces.ed.gov/programs/edge/geographic/schoollocations
dir = 'data/gis/schools/'
schools_public  = st_read(paste0(dir, 'EDGE_GEOCODE_PUBLICSCH_2021/EDGE_GEOCODE_PUBLICSCH_2021.shp'))
schools_private = st_read(paste0(dir, 'EDGE_GEOCODE_PRIVATESCH_1920/EDGE_GEOCODE_PRIVATESCH_1920.shp'))
schools_postsec = st_read(paste0(dir, 'EDGE_GEOCODE_POSTSECONDARYSCH_2021/EDGE_GEOCODE_POSTSECSCH_2021.shp'))
schools_public  = st_transform(schools_public, crs)
schools_private = st_transform(schools_private, crs)
schools_postsec = st_transform(schools_postsec, crs)

schools_public = st_crop(schools_public, Ldn_gt55_bounds)
schools_private = st_crop(schools_private, Ldn_gt55_bounds)
schools_postsec = st_crop(schools_postsec, Ldn_gt55_bounds)

schools_public  = schools_public[, c('NAME', 'ZIP', 'LAT', 'LON')]
schools_public$TYPE = 'PUBLIC'
schools_private = schools_private[, c('NAME', 'ZIP', 'LAT', 'LON')]
schools_private$TYPE = 'PRIVATE'
schools_postsec = schools_postsec[, c('NAME', 'ZIP', 'LAT', 'LON')]
schools_postsec$TYPE = 'POSTSECONDARY'

schools = bind_rows(bind_rows(schools_public, schools_private), schools_postsec)
schools$TYPE = factor(schools$TYPE)
schools_affected = st_intersection(schools, Ldn_gt55) # affected schools
schools_affected$Ldn55 = TRUE
schools_unaffected = schools[!(schools$NAME %in% schools_affected$NAME),]
schools_unaffected$Ldn55 = FALSE
schools = bind_rows(schools_affected, schools_unaffected)

# Affected schools
schools_affected$NAME
mapview(Ldn_gt55, zcol='Level', layer.name='DNL') + mapview(population_bg, col.regions=list('white')) +
  mapview(schools, zcol='Ldn55', col.regions=list('gray', 'red')) +
  mapview( sites_with_events, xcol='Longitude', ycol='Latitude', layer.name = 'Sites', crs=crs, grid=F) +
  mapview(get_flighttracks())

## Cognitive development in children -------------------------------------------

# WHO guidelines: Reading and oral comprehension 1–2-month delay per 5 dB (outdoor Lden) increase. Relevant RR increase of impaired reading and oral comprehension at 55 dB (1 month delay in reading and oral comprehension, as assessed by standardized tests)
# https://nap.nationalacademies.org/catalog/22433/assessing-aircraft-noise-conditions-affecting-student-learning-volume-1-final-report
# "Evidence for a relevant RR increase of impaired reading and oral comprehension at 55 dB Lden was rated moderate quality" (WHO Environmental Noise Guidelines, 2019)

unique(data_metrics[data_metrics$Lden>=55, 'ID'])

## Speech intelligibility / interference ---------------------------------------

# ANSI 12.3 states that the criteria for allowable background noise level can be relaxed for irregular noise events. This is because speech is impaired only for the short time when the aircraft noise is close to its maximum. Consequently, when the background noise level of the noisiest hour is dominated by aircraft noise, the indoor criteria can be increased. The Leq of 35 dB for continuous background noise can be increased by 5 dB to an Leq of 40 dB. However, the noise level cannot exceed 40 dB for more than 10% of the noisiest hour. This is for a room that's less than 20,000 cubic feet.

# https://www.noisequest.psu.edu/noiseeffects-speech.html
# SEL has been recommended by some as a better choice for estimating speech interference from aircraft overflights indoors. A maximum SEL of 64 dB is suggested. A 26 dB noise reduction is assumed when you move indoors from outdoors. So, a 64 dB SEL indoors is about equal to 90 dB SEL outdoors. Aircraft events with outdoor SEL values greater than 90 dB would disrupt indoor speech communication. The research indicates that speakers using a casual vocal effort can achieve 95% intelligibility when indoor SEL values did not exceed 60 dB. This translates to an approximately 50 dB Lmax.

# Measured SEL near schools
# Near Coupeville Elementary and Coupeville High
# TODO: exact distance, and only consider school hours
data_events[data_events$ID=='NwbH' & data_events$SEL >= 90, ]
# Near Crescent Harbor Elementary and Olympic View Elementary
# TODO: exact distance, and only consider school hours
data_events[data_events$ID=='2B_T' & data_events$SEL >= 90, ]

# TODO: Modeled SEL for flight tracks near schools
