library(mapview)
library(sf)
library(tidyverse)

setwd('~/Google Drive/My Drive/Research/PHI Aircraft Noise Project/')
csv = read.csv('PHI Monitoring Data Summary - Summary.csv')

data = na.omit(csv[,c('Organization','Site.Name','Latitude','Longitude','Dates')])

mapviewOptions(legend.pos='bottomright')
mapview(
  data,
  xcol='Longitude',
  ycol='Latitude',
  zcol='Organization',
  crs=4269,
  grid=FALSE,
  col.regions=c('sienna','gold','blue','green3','salmon','darkturquoise','purple3'),
  legend=TRUE,
  layer.name = 'Organization'
)
