# Spatial distribution of noise exposure at population level

source('global.R')
source('analysis/spatial_distribution.R')

library(tidycensus)
census_api_key('a9d9f05e0560c7fadaed6b4168bedc56d0e4686d')

population_bg = get_decennial(
  geography = 'block group',
  variables = 'P1_001N', # population
  state = 'WA',
  year = 2020,
  geometry = TRUE
)
# mapview(population_bg, zcol = 'value')

###################################################################################################
# Ldn / DNL

# # Example with Coupeville block group
# ex_bg = population_bg[population_bg$GEOID==530299710001,]
# ex_intersection = st_intersection(ex_bg, contours_Ldn)
# mapview(ex_intersection, zcol='Level')
# areas = as.numeric(st_area(ex_intersection) / st_area(ex_bg)) # proportion
# ex_intersection$subpopulation = ex_intersection$value * areas
# mapview(ex_intersection, zcol='subpopulation')
# level_areas = left_join(data.frame(Level=contours_Ldn$Level), data.frame(Level=ex_intersection$Level, Area=areas), by = join_by(Level)) %>% replace(is.na(.), 0)

# Total exposure, measured as intersection of population layer with contours
if (!exists('exposure_Ldn')) {
  exposure_Ldn = st_intersection(population_bg[,c('GEOID', 'NAME', 'value', 'geometry')], contours_Ldn)
  mapview(exposure_Ldn, zcol='Level', layer.name='DNL') + mapview(population_bg, col.regions=list('white'))
  
  # Population total ('subpopulation') for an intersection feature, calculated from proportion of total block group population
  exposure_Ldn$subpopulation = 0
  for (r in 1:nrow(exposure_Ldn)) {
    i = exposure_Ldn[r,]
    bg = population_bg[population_bg$GEOID==i$GEOID,]
    proportion = as.numeric(st_area(i) / st_area(bg))
    exposure_Ldn[r, 'subpopulation'] = round(proportion * bg$value) # proportion of total block group population
  }
}

###################################################################################################
# Lnight

# Total exposure, measured as intersection of population layer with contours
if (!exists('exposure_Lnight')) {
  exposure_Lnight = st_intersection(population_bg[,c('GEOID', 'NAME', 'value', 'geometry')], contours_Lnight)
  mapview(exposure_Lnight, zcol='Level', layer.name='Lnight') + mapview(population_bg, col.regions=list('white'))
  
  # Population total ('subpopulation') for an intersection feature, calculated from proportion of total block group population
  exposure_Lnight$subpopulation = 0
  for (r in 1:nrow(exposure_Lnight)) {
    i = exposure_Lnight[r,]
    bg = population_bg[population_bg$GEOID==i$GEOID,]
    proportion = as.numeric(st_area(i) / st_area(bg))
    exposure_Lnight[r, 'subpopulation'] = round(proportion * bg$value) # proportion of block group pop
  }
}

###################################################################################################
# Leq24

# Total exposure, measured as intersection of population layer with contours
if (!exists('exposure_Leq24')) {
  exposure_Leq24 = st_intersection(population_bg[,c('GEOID', 'NAME', 'value', 'geometry')], contours_Leq24)
  mapview(exposure_Leq24, zcol='Level', layer.name='Leq24') + mapview(population_bg, col.regions=list('white'))
  
  # population total for an intersection feature, estimated as proportion of total block group population
  exposure_Leq24$subpopulation = 0
  for (r in 1:nrow(exposure_Leq24)) {
    i = exposure_Leq24[r,]
    bg = population_bg[population_bg$GEOID==i$GEOID,]
    proportion = as.numeric(st_area(i) / st_area(bg))
    exposure_Leq24[r, 'subpopulation'] = round(proportion * bg$value) # proportion of block group pop
  } 
}

########################################################################################################
# Insights
# Note that these numbers may be conservative estimates due to the lack of evening-time penalty in DNL calculations, and depending on the exposure-response function used.

# Population exposure per 5 dB
level_pops = st_drop_geometry(exposure_Ldn) %>% group_by(Level) %>% summarise(sum_pop= sum(subpopulation))
level_pops$Level = factor(level_pops$Level)
ggplot(level_pops, aes(x=Level, y=sum_pop)) + geom_bar(position='dodge', stat='identity') + ggtitle('Estimated population exposed per 5dB Ldn') + xlab('Ldn (dB)') + ylab('Estimated population')

# WHO - "For average noise exposure, the GDG strongly recommends reducing noise levels produced by aircraft below 45 dB Lden, as aircraft noise above this level is associated with adverse health effects." The U.S. EPA recommends an average 24-hr exposure limit of 55 DNL (dBA) to protect the public from all adverse effects on health and welfare in residential areas
lden_impact_threshold = 45
mapview(exposure_Ldn[as.numeric(exposure_Ldn$Level)>=lden_impact_threshold,], zcol='Level') +
  mapview(population_bg, col.regions=list('white'))

# Total area of noise exposure associated with adverse health effects (note this includes water)
contour_area = st_area(st_make_valid(st_union(contours_Ldn[contours_Ldn$Level>=lden_impact_threshold,])))
units::set_units(contour_area, km^2)
units::set_units(contour_area, mi^2)

# Estimated number of people subject to noise exposure levels associated with adverse health effects
sum(st_drop_geometry(exposure_Ldn[exposure_Ldn$Level>=lden_impact_threshold, ])$subpopulation)
