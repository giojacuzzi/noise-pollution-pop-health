library(mapview)
library(sf)
library(tidyverse)

data_sites = read.csv('data/sites.csv')

# Organization map
mapviewOptions(legend.pos='bottomright')
mapview(
  data_sites,
  xcol='Longitude',
  ycol='Latitude',
  zcol='Org',
  crs=4269,
  grid=FALSE,
  col.regions=c('sienna','gold','blue','green3','salmon','darkturquoise','purple3'),
  legend=TRUE,
  layer.name = 'Organization'
)
