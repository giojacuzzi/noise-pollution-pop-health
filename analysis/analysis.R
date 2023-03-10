source('global.R')
source('data/metrics/metrics.R')
library(mapview)
library(leafem)

# Monitoring site map  ---------------------------------------------------------

# Only use Whidbey Island sites with present SPL data
data_sites = get_data_sites()
data_sites = na.omit(data_sites)
data_sites = data_sites[data_sites$Region=='Whidbey Island',]
data_sites = data_sites[data_sites$Data=='SPL' | data_sites$Data=='SPL,AUDIO',]

mapviewOptions(legend.pos='bottomright')
mapview(
  data_sites, xcol='Longitude', ycol='Latitude', zcol='Org',
  layer.name = 'Organization', crs=4269, grid=F, legend=T,
  col.regions=c('darkgoldenrod2', 'navy', 'green3', 'darkturquoise')
) %>% addStaticLabels(label=data_sites$ID, direction='top')