library(dplyr)
library(sf)
library(mapview)
library(ggplot2)
library(ggrepel)
library(tidycensus)
census_api_key('a9d9f05e0560c7fadaed6b4168bedc56d0e4686d')
crs = 'NAD83'

# https://geo.wa.gov/datasets/a0ddbd4e0e2141b3841a6a42ff5aff46_0/explore?location=47.981523%2C-122.678861%2C8.16&style=MASTER_CAT

# Land use
dir = '/Volumes/SAFS Work/PHI/Land Use'
landuse  = st_read(paste0(dir, '/General_Land_Use_Final_Dataset.shp'))
landuse  = st_transform(landuse, crs)
landuse = landuse[, c('OBJECTID', 'NAME', 'MASTER_CAT')]
landuse = st_make_valid(landuse)

bounds_x = c(-123.72285, -121.23736) # [min, max]
bounds_y = c(47.67722, 48.61497)
bounds = data.frame(
  x = c(bounds_x[1], bounds_x[1], bounds_x[2], bounds_x[2]),
  y = c(bounds_y[1], bounds_y[2], bounds_y[2], bounds_y[1])
)
landuse = st_crop(landuse, c(xmin=bounds_x[1], ymin=bounds_y[1], xmax=bounds_x[2], ymax=bounds_y[2]))
landuse$MASTER_CAT = factor(landuse$MASTER_CAT)

# library(forcats)
# fct_collapse(landuse$MASTER_CAT,
#              AB = c('A','B'),
#              DE = c('D','E')
# )

flighttracks = st_read('data/flight_ops/modeling/baseops/Aggregated/DNL/NASWI_Aggregated_Noisemap - Aggregate_ContourLine_Lines - VALID/_FlightTracks_Lines.shp')
flighttracks = st_set_crs(flighttracks, 'NAD83')
runways = st_read('data/flight_ops/modeling/baseops/Aggregated/DNL/NASWI_Aggregated_Noisemap - Aggregate_ContourLine_Lines - VALID/_Runways_Lines.shp')
runways = st_set_crs(runways, 'NAD83')

mapview(flighttracks, lwd = 0.5) + mapview(runways, lwd = 5, )
