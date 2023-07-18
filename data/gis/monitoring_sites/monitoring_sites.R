source('global.R')

data_sites = get_data_sites()
data_metrics = get_data_metrics()

mapview(
  data_sites,
  xcol='Longitude', ycol='Latitude', zcol='Org',
  layer.name = 'Organization', crs=4269, grid=F, legend=T,
  col.regions=c('darkgoldenrod2', 'navy', 'green3', 'darkturquoise')
) %>% addStaticLabels(label=sites_with_events$ID, direction='top')
