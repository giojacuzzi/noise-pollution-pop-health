## Noise characterization
# How is the noise quantitatively characterized?

source('global.R')
source('data/metrics/metrics.R')
library(mapview)
library(leafem)

data_sites  = get_data_sites()
data_events = get_data_events()

## Monitoring site map  --------------------------------------------------------
# Display all sites that have calculated events

sites_with_events = data_sites[data_sites$ID %in% unique(data_events$ID),]
mapviewOptions(legend.pos='bottomright')
mapview(
  sites_with_events,
  xcol='Longitude', ycol='Latitude', zcol='Org',
  layer.name = 'Organization', crs=4269, grid=F, legend=T,
  col.regions=c('darkgoldenrod2', 'navy', 'green3', 'darkturquoise')
) %>% addStaticLabels(label=sites_with_events$ID, direction='top')
