### Plot and map results
source('global.R')
library('ggplot2')
library('viridis')
library('mapview')

data_metrics = get_data_metrics()

# Organization map -------------------------------------------------------------
mapviewOptions(legend.pos='bottomright')
mapview(
  get_data_sites(),
  xcol='Longitude', ycol='Latitude', zcol='Org',
  crs=4269, grid=FALSE, legend=TRUE,
  col.regions=c('sienna','gold','blue','green3','salmon','darkturquoise','purple3'),
  layer.name = 'Organization'
)

# Ldn median map -----------------------------------------------------------------
median_dnl = tapply(data_metrics[!is.na(data_metrics$Ldn),'Ldn'], data_metrics[!is.na(data_metrics$Ldn),'ID'], median)

mapview(
  merge(get_data_sites(), data.frame(dB=c(t(median_dnl)), ID=rownames(median_dnl)), all=TRUE),
  xcol='Longitude', ycol='Latitude', zcol='dB',
  cex='dB', crs=4269, grid=FALSE, legend=TRUE,
  layer.name = 'Median Ldn (dBA)'
)

# Level maximums and peaks -----------------------------------------------------
data_metrics_max = data_metrics
data_metrics_max[is.na(data_metrics_max)] = 0
max_Lmax = tapply(data_metrics_max$Lmax, data_metrics_max$ID, max)
max_LCpeak = tapply(data_metrics_max$LCpeak, data_metrics_max$ID, max)
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

# Time Breakdowns (NAVY only) --------------------------------------------------

day_abbr = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
# season_abbr = c('Spring', 'Summer', 'Fall', 'Winter')
daily_levels = na.omit(data_metrics[data_metrics$Org=='NAVY', ])
daily_levels = cbind(daily_levels, Day=weekdays(as.POSIXct(daily_levels$Date, tz='UTC'), abbreviate=T))
daily_levels$Day = factor(daily_levels$Day, levels=day_abbr)

daily_levels = cbind(daily_levels, Month=months(as.POSIXct(daily_levels$Date, tz='UTC'), abbreviate=T))
daily_levels$Month = factor(daily_levels$Month, levels=month.abb)
# daily_levels = cbind(daily_levels, Season=cut(as.numeric(daily_levels$Month), breaks=c(12,2,5,8,11), labels=season_abbr, right=T))
# daily_levels$Season = factor(daily_levels$Season, levels=season_abbr)
daily_levels = cbind(daily_levels, Period=daily_levels$Month)
levels(daily_levels$Period)[levels(daily_levels$Period)=='Mar'] <- 'Mar/Apr'
levels(daily_levels$Period)[levels(daily_levels$Period)=='Apr'] <- 'Mar/Apr'

# Ldn per day
ggplot(daily_levels[order(daily_levels$Day), ], aes(x=Day, y=Ldn, fill=Org)) +
  geom_boxplot(alpha=0.4) +
  labs(title='Ldn per day across all Navy sites', x ='Day', y ='Ldn (dBA)') +
  geom_hline(yintercept=65, linetype='dotted', colour='red') # HUD / FAA

# Median Leq hourly heatmap per day
data_hour_day_levels = data.frame()
for (hour in 0:23) {
  leq_hr = paste('Leq', formatC(hour, width=2, flag='0'), sep='')
  result = tapply(daily_levels[,leq_hr], INDEX=daily_levels$Day, FUN=median)
  result = data.frame(Hour=hour, Day=names(result), Leq=result)
  rownames(result) = c()
  data_hour_day_levels = rbind(data_hour_day_levels, result)
}
data_hour_day_levels$Day = factor(data_hour_day_levels$Day, levels = day_abbr)

ggplot(data_hour_day_levels[order(as.numeric(data_hour_day_levels$Day)),], aes(x=Hour, y=Day, fill=Leq)) +
  geom_tile() +
  scale_fill_viridis(option='A') +
  labs(title='Median Leq heatmap across all Navy sites', x='Hour', y='Day') +
  scale_x_continuous('Hour', labels = as.character(0:23), breaks = 0:23)

# Leq per hour, per day
for (day in day_abbr) {
  fdsa = data.frame()
  for (hour in 0:23) {
    leq_hr = paste('Leq', formatC(hour, width=2, flag='0'), sep='')
      levels_for_day_hour = daily_levels[daily_levels$Day==day,leq_hr]
      result = data.frame(Hour=hour, Day=day, Leq=levels_for_day_hour)
      fdsa = rbind(fdsa, result)
    # }
  }
  fdsa$Hour = factor(fdsa$Hour, levels = 0:23)
  print(ggplot(fdsa, aes(x=Hour, y=Leq)) +
    geom_boxplot(alpha=0.4) +
    coord_cartesian(ylim=c(20,100)) +
    labs(title=paste(day, 'hourly Leq across all Navy sites'), x ='Hour', y ='Leq (dBA)'))
}
fdsa = data.frame()
for (hour in 0:23) {
  leq_hr = paste('Leq', formatC(hour, width=2, flag='0'), sep='')
  for (day in unique(daily_levels$Day)) {
    levels_for_day_hour = daily_levels[daily_levels$Day==day,leq_hr]
    result = data.frame(Hour=hour, Day=day, Leq=levels_for_day_hour)
    fdsa = rbind(fdsa, result)
  }
}
fdsa$Day = factor(fdsa$Day, levels = day_abbr)
fdsa$Hour = factor(fdsa$Hour, levels = 0:23)
ggplot(fdsa, aes(x=Day, y=Leq, fill=Hour)) +
  geom_boxplot(alpha=0.4) +
  labs(title='Hourly Leq per day across all Navy sites', x ='Day', y ='Leq (dBA)')

# Lmax per day
ggplot(daily_levels[order(daily_levels$Day), ], aes(x=Day, y=Lmax, fill=Org)) +
  geom_boxplot(alpha=0.4) +
  labs(title='Lmax per day across all Navy sites', x ='Day', y ='Lmax (dBA)') +
  geom_hline(yintercept=65, linetype='dotted', colour='red') # HUD / FAA

# Ldn per measurement period
ggplot(daily_levels, aes(x=Period, y=Ldn, fill=Org)) +
  geom_boxplot(alpha=0.4) +
  labs(title='Ldn per measurement period across all Navy sites', x ='Period (Month)', y ='Ldn (dBA)')

# Ldn per day, per measurement period
ggplot(daily_levels, aes(x=Day, y=Ldn, fill=Period)) +
  geom_boxplot(alpha=0.4) +
  labs(title='Ldn per day, per measurement period across all Navy sites', x ='Period', y ='Ldn (dBA)')
