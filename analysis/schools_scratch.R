library(dplyr)
library(sf)
library(mapview)
library(ggplot2)
library(ggrepel)
library(tidycensus)
census_api_key('a9d9f05e0560c7fadaed6b4168bedc56d0e4686d')

# https://nces.ed.gov/programs/edge/geographic/schoollocations

# Public schools
dir = '/Volumes/SAFS Work/PHI/Schools'
schools_public  = st_read(paste0(dir, '/EDGE_GEOCODE_PUBLICSCH_2021/Shapefile/EDGE_GEOCODE_PUBLICSCH_2021.shp'))
schools_private = st_read(paste0(dir, '/EDGE_GEOCODE_PRIVATESCH_1920/Shapefile/EDGE_GEOCODE_PRIVATESCH_1920.shp'))
schools_postsec = st_read(paste0(dir, '/EDGE_GEOCODE_POSTSECONDARYSCH_2021/Shapefile/EDGE_GEOCODE_POSTSECSCH_2021.shp'))

crs = 'NAD83'
schools_public  = st_transform(schools_public, crs)
schools_private = st_transform(schools_private, crs)
schools_postsec = st_transform(schools_postsec, crs)

bounds_x = c(-123.72285, -121.23736) # [min, max]
bounds_y = c(47.67722, 48.61497)
bounds = data.frame(
  x = c(bounds_x[1], bounds_x[1], bounds_x[2], bounds_x[2]),
  y = c(bounds_y[1], bounds_y[2], bounds_y[2], bounds_y[1])
)

schools_public = st_crop(schools_public, c(xmin=bounds_x[1], ymin=bounds_y[1], xmax=bounds_x[2], ymax=bounds_y[2]))
schools_private = st_crop(schools_private, c(xmin=bounds_x[1], ymin=bounds_y[1], xmax=bounds_x[2], ymax=bounds_y[2]))
schools_postsec = st_crop(schools_postsec, c(xmin=bounds_x[1], ymin=bounds_y[1], xmax=bounds_x[2], ymax=bounds_y[2]))

schools_public  = schools_public[, c('NAME', 'ZIP', 'LAT', 'LON')]
schools_public$TYPE = 'PUBLIC'
schools_private = schools_private[, c('NAME', 'ZIP', 'LAT', 'LON')]
schools_private$TYPE = 'PRIVATE'
schools_postsec = schools_postsec[, c('NAME', 'ZIP', 'LAT', 'LON')]
schools_postsec$TYPE = 'POSTSECONDARY'

mapview(schools_public)
mapview(schools_private)
mapview(schools_postsec)

schools = bind_rows(bind_rows(schools_public, schools_private), schools_postsec)
schools$TYPE = factor(binded$TYPE)
mapview(schools, zcol='TYPE')
