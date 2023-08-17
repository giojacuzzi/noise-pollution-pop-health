source('global.R')
source('analysis/population_exposure.R')

land_use = st_make_valid(st_read('/Volumes/SAFS Work/PHI/Land Use/General_Land_Use_Final_Dataset.shp'))
land_use_levels = st_intersection(contours_Ldn, st_transform(land_use, crs))

# https://www.hudexchange.info/resource/313/hud-noise-guidebook/
labels = c('Clearly Acceptable', 'Normally Acceptable', 'Normally Unacceptable', 'Clearly Unacceptable')
# Upper label limits per land use type
residential = c(60,65,75) # also schools, libraries, churches, hospitals, nursing homes
parks       = c(55,65,75) # also playgrounds
water_rec   = c(60,70,80) # also cemeteries, golf courses
commercial  = c(65,75,80) # also offices, retail, restaurants
industrial  = c(70,80,85) # also wholesale, manufacturing, utilities
livestock   = c(60,75,80)
agriculture = c(75,NA,NA)
prow        = c(75,85,NA)
natural_rec = c(60,75,85)