source('global.R')
source('data/metrics/metrics.R')
library(mapview)
library(leafem)

data_sites   = get_data_sites()
data_metrics = get_data_metrics()

## Monitoring site map  --------------------------------------------------------
# Display all sites that have calculated metrics
mapviewOptions(legend.pos='bottomright')
mapview(
  data_sites[data_sites$ID %in% unique(data_metrics$ID),],
  xcol='Longitude', ycol='Latitude', zcol='Org',
  layer.name = 'Organization', crs=4269, grid=F, legend=T,
  col.regions=c('darkgoldenrod2', 'navy', 'green3', 'darkturquoise')
) %>% addStaticLabels(label=data_sites$ID, direction='top')
