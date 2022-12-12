library(mapview)
library(sf)
library(tidyverse)

data_sites = read.csv('data/sites.csv')

# Organization map
mapviewOptions(legend.pos='bottomright')
mapview(
  data_sites,
  xcol='Longitude', ycol='Latitude', zcol='Org',
  crs=4269, grid=FALSE, legend=TRUE,
  col.regions=c('sienna','gold','blue','green3','salmon','darkturquoise','purple3'),
  layer.name = 'Organization'
)

data_metrics = read.csv('data/metrics/metrics_navy.csv')
data_metrics = na.omit(data_metrics)

max_dnl = tapply(data_metrics$Ldn, data_metrics$ID, max)
data_max_dnl = data.frame(dB=c(t(max_dnl)), ID=rownames(max_dnl))

# Ldn max map
mapviewOptions(legend.pos='bottomright')
mapview(
  merge(data_sites, data_max_dnl, all=TRUE),
  xcol='Longitude', ycol='Latitude', zcol='dB',
  cex='dB', crs=4269, grid=FALSE, legend=TRUE,
  layer.name = 'Max Ldn (dB)'
)
