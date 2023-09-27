## Cognitive development in children and speech intelligibility

source('global.R')
source('metrics/metrics.R')
source('metrics/thresholds.R')
source('simulation/contours.R')

data_sites   = get_data_sites()
data_metrics = get_data_metrics()
data_events = get_data_events()

sites_with_events = data_sites[data_sites$ID %in% unique(data_events$ID),]

output_path = paste0(here::here(), '/analysis/_output')

###############################################################################################
# Children's learning and comprehension

# Ldn >= 55 dB poses risk of inhibited reading skills and oral comprehension in children (WHO, RANCH). As contours are calculated for average annual day, we recalculate contour associated with school year.
n_school_days = 180
threshold_school_year = LeqTotal(c(rep(threshold_reading_comprehension_Lden, n_school_days), rep(0, 365 - n_school_days)))

# The difference between the annual (365 day) exposure and the school year exposure
threshold_delta = round(threshold_reading_comprehension_Lden - threshold_school_year)

msg('Equivalent ', threshold_reading_comprehension_Lden,' dB Lden annual exposure for ', n_school_days,'-day school year: ', Ldn_target, ' dB', sep='')

# Relabel contours according to school year exposure
contours_reading_comprehension = get_contours_Ldn()
contours_reading_comprehension$Level = contours_reading_comprehension$Level - threshold_delta
contours_reading_comprehension_bounds = st_bbox(contours_reading_comprehension)

# https://nces.ed.gov/programs/edge/geographic/schoollocations
dir = 'data/gis/schools/'
schools_public  = st_read(paste0(dir, 'EDGE_GEOCODE_PUBLICSCH_2021/EDGE_GEOCODE_PUBLICSCH_2021.shp'))
schools_private = st_read(paste0(dir, 'EDGE_GEOCODE_PRIVATESCH_1920/EDGE_GEOCODE_PRIVATESCH_1920.shp'))
schools_postsec = st_read(paste0(dir, 'EDGE_GEOCODE_POSTSECONDARYSCH_2021/EDGE_GEOCODE_POSTSECSCH_2021.shp'))
schools_public  = st_transform(schools_public, crs)
schools_private = st_transform(schools_private, crs)
schools_postsec = st_transform(schools_postsec, crs)

schools_public = st_crop(schools_public, contours_reading_comprehension_bounds)
schools_private = st_crop(schools_private, contours_reading_comprehension_bounds)
schools_postsec = st_crop(schools_postsec, contours_reading_comprehension_bounds)

schools_public  = schools_public[, c('NAME', 'ZIP', 'LAT', 'LON')]
schools_public$TYPE = 'PUBLIC'
schools_private = schools_private[, c('NAME', 'ZIP', 'LAT', 'LON')]
schools_private$TYPE = 'PRIVATE'
schools_postsec = schools_postsec[, c('NAME', 'ZIP', 'LAT', 'LON')]
schools_postsec$TYPE = 'POSTSECONDARY'
schools = bind_rows(bind_rows(schools_public, schools_private), schools_postsec)
schools$TYPE = factor(schools$TYPE)

schools = st_intersection(schools, contours_reading_comprehension)
schools$AtRisk = F
schools[schools$Level >= threshold_reading_comprehension_Lden, 'AtRisk'] = T

# Remove web-based, homeschooling, unincorporated, and duplicate programs
schools_to_remove = c('Oak Harbor Virtual Academy', 'Igrad Academy', 'Homeconnection', 'Open Den', 'Island Juvenile Detention Education Program')
schools = schools[!(schools$NAME %in% schools_to_remove), ]
schools = schools[order(schools$Level, decreasing=T), ]

# Map results
# mapview(contours_reading_comprehension, zcol='Level', layer.name='DNL', col.regions=viridis_pal(option='C')(length(seq(45,90,5)))) + mapview(schools, zcol='AtRisk', col.regions=list('gray', 'red')) + mapview(data_sites, xcol='Longitude', ycol='Latitude', crs=4269, grid=F)

# Format results and write to csv
s = st_drop_geometry(schools[, c('NAME', 'ZIP', 'TYPE', 'Level', 'LAT', 'LON', 'AtRisk')])
s = s[s$AtRisk==T,]
s$TYPE = str_to_title(s$TYPE)
s$NAME = str_to_title(s$NAME)
s$NAME = gsub(' - Whidbey Island', '', s$NAME)
s$Level5dB = floor(as.numeric(s$Level) / 5) * 5 # in increments of 5 dB
s$ZIP = substr(s$ZIP, 1, 5)
s = s[order(s$Level5dB, decreasing=T), ]
s = s[!(s$NAME %in% schools_to_remove), ]
s$Delay = gsub('1-1', '1', paste0(
  as.character((s$Level5dB - 55) / 5 * 1 + 1), '-', # 1 month delay at 55, 1-2 month delay per 5 dB increase
  as.character((s$Level5dB - 55) / 5 * 2 + 1)
))

schools_affected = na.omit(s)
schools_affected = schools_affected[, c('NAME', 'Level', 'Delay')]
names(schools_affected) = c('School', 'Ldn', 'Delay')
write.csv(schools_affected, glue(output_path, '/schools_affected.csv'), row.names = F)

print(schools_affected)

# Additional schools within 5 dB of the threshold
nrow(schools[schools$Level >= (threshold_reading_comprehension_Lden - 5) & schools$AtRisk==F,])

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
# Only consider school hours -> 8:00 am to 3:59 pm
# Near Coupeville Elementary and Coupeville High
# TODO: exact distance, and only consider school hours

# At NbwH, the field monitoring site nearest Coupeville Elementary (347 m, 1,139 ft) and Coupeville High (~264 m, 867 ft),
# aircraft noise events surpassed 103 dB SEL and 94 dB Lmax.
events_NwbH = data_events[data_events$ID=='NwbH' &
              (as.numeric(data_events$Hour) >=  8) &
              (as.numeric(data_events$Hour) <=  15) &
              !(data_events$Day %in% c('Sat', 'Sun')), ]
head(events_NwbH[order(events_NwbH$SEL, decreasing=T), ])
head(events_NwbH[order(events_NwbH$LAeq_Lmax, decreasing=T), ])

# At 2B_T, the field monitoring site nearest Crescent Harbor Elementary (~1 km, 3,293 ft),
# aircraft noise events surpassed 113 dB SEL and 103 dB Lmax.
events_2B_T = data_events[data_events$ID=='2B_T' &
                            (as.numeric(data_events$Hour) >=  8) &
                            (as.numeric(data_events$Hour) <=  15) &
                            !(data_events$Day %in% c('Sat', 'Sun')), ]
head(events_2B_T[order(events_2B_T$SEL, decreasing=T), ])
head(events_2B_T[order(events_2B_T$LAeq_Lmax, decreasing=T), ])

# TODO: Modeled SEL for flight tracks near schools
