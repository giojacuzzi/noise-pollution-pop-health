source('global.R')
source('data/metrics/metrics.R')
library(mapview)

# Monitoring site map  ---------------------------------------------------------

# Only use Whidbey Island sites with present SPL data
data_sites = get_data_sites()
data_sites = na.omit(data_sites)
data_sites = data_sites[data_sites$Region=='Whidbey Island',]
data_sites = data_sites[data_sites$Data=='SPL' | data_sites$Data=='SPL,AUDIO',]

mapviewOptions(legend.pos='bottomright')
mapview(
  data_sites,
  xcol='Longitude', ycol='Latitude', zcol='Org',
  crs=4269, grid=FALSE, legend=TRUE,
  col.regions=c('darkgoldenrod2', 'navy', 'green3', 'darkturquoise'),
  layer.name = 'Organization'
)
