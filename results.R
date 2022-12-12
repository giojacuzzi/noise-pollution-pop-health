library('ggplot2')
library('viridis')
library('mapview')

data_sites = read.csv('data/sites.csv')
data_metrics = read.csv('data/metrics/metrics_navy.csv')
data_metrics = na.omit(data_metrics)

# Organization map
mapviewOptions(legend.pos='bottomright')
mapview(
  data_sites,
  xcol='Longitude', ycol='Latitude', zcol='Org',
  crs=4269, grid=FALSE, legend=TRUE,
  col.regions=c('sienna','gold','blue','green3','salmon','darkturquoise','purple3'),
  layer.name = 'Organization'
)

# Day-night average means and maximums -----------------------------------------
mean_dnl = tapply(data_metrics$Ldn, data_metrics$ID, mean)
mean_denl = tapply(data_metrics$Lden, data_metrics$ID, mean)
mean_day = tapply(data_metrics$Lden_Lday, data_metrics$ID, mean)
mean_evening = tapply(data_metrics$Lden_Levening, data_metrics$ID, mean)
mean_night = tapply(data_metrics$Lden_Lnight, data_metrics$ID, mean)

max_dnl = tapply(data_metrics$Ldn, data_metrics$ID, max)
max_denl = tapply(data_metrics$Lden, data_metrics$ID, max)
max_day = tapply(data_metrics$Lden_Lday, data_metrics$ID, max)
max_evening = tapply(data_metrics$Lden_Levening, data_metrics$ID, max)
max_night = tapply(data_metrics$Lden_Lnight, data_metrics$ID, max)

# Ldn max map
mapviewOptions(legend.pos='bottomright')
mapview(
  merge(data_sites, data.frame(dB=c(t(max_dnl)), ID=rownames(max_dnl)), all=TRUE),
  xcol='Longitude', ycol='Latitude', zcol='dB',
  cex='dB', crs=4269, grid=FALSE, legend=TRUE,
  layer.name = 'Max Ldn (dB)'
)

# Day-night average grouped barplot
dnl_denl = data.frame(mean_dnl, mean_denl, max_dnl, max_denl)
ggplot(
  data.frame(dB = c(t(dnl_denl[,])),
             id = rep(rownames(dnl_denl),each=4),
             metric = c('Mean Ldn','Mean Lden','Max Ldn','Max Lden')),
  aes(fill=metric, y=dB, x=id)) + 
  geom_bar(position='dodge', stat='identity') +
  theme_minimal() + 
  labs(x='Site', y='dB', title='Day-night average means and maximums') +
  scale_fill_manual('', values=viridis(4))

# Level maximums and peaks -----------------------------------------------------
max_Lmax = tapply(data_metrics$Lmax, data_metrics$ID, max)
max_LCpeak = tapply(data_metrics$LCpeak, data_metrics$ID, max)

l_maxpeak = data.frame(max_Lmax, max_LCpeak)
ggplot(
  data.frame(dB = c(t(l_maxpeak[,])),
             id = rep(rownames(l_maxpeak),each=2),
             metric = c('Lmax (dBA)','Lpeak (dBC)')),
  aes(fill=metric, y=dB, x=id)) + 
  geom_bar(position='dodge', stat='identity') +
  theme_minimal() + 
  labs(x='Site', y='dB', title='Level maximums and peaks') +
  scale_fill_manual('',values=viridis(4))


# Dates by Ldn -----------------------------------------------------------------
dates_by_ldn = data_metrics[with(data_metrics,order(-Ldn)),]
dates_by_ldn[1:20]
