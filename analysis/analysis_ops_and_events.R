# Bar plots of noise event quantity vs hour of day / day of week (per-site, per-period)

library(ggplot2)
library(patchwork)
library(stringr)
theme_set(theme_bw())

hours = str_pad(0:23, 2, pad = '0')
days  = c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')

get_time_period_for_hours = function(hours) {
  return(cut(as.numeric(hours),
             breaks=c(-1,6,18,22,23),
             labels=c('Night','Day','Evening','Night')))
}

sites = read.csv('data/sites/sites.csv')
sites = sites[sites$Org=='NAVY' & (sites$Region=='Ault Field' | sites$Region=='OLF Coupeville'), ]
events = read.csv('data/events/output/events.csv')

events$StartTime       = as.POSIXct(events$StartTime)
events$Hour       = strftime(events$StartTime, format='%H')
events$TimePeriod = get_time_period_for_hours(events$Hour)
events$Day        = factor(weekdays(events$StartTime, abbreviate=T), levels=days)

plots_events_per_site_daily = list()
plots_events_per_site_hourly = list()
for (s in 1:nrow(sites)) {
  site = sites[s,'ID']
  events_site = events[events$SiteID==site,]
  events_site$MonitoringPeriod = as.factor(events_site$MonitoringPeriod)
  
  # Operations by weekday
  pdata_daily = data.frame()
  for (p in 1:4) {
    daily_ops = c()
    for (day in days) {
      daily_ops = c(daily_ops, sum(events_site$Day==day & events_site$MonitoringPeriod==p))
    }
    pdata_daily = rbind(pdata_daily, data.frame(
      Day=factor(days, levels=days),
      NumOps=daily_ops,
      MonitoringPeriod=p
    ))
  }
  pdata_daily$MonitoringPeriod = as.factor(pdata_daily$MonitoringPeriod)
  
  p = ggplot(data=pdata_daily[order(pdata_daily$Day), ], aes(fill=MonitoringPeriod, color=MonitoringPeriod, group=MonitoringPeriod, x=Day, y=NumOps)) +
    geom_bar(position='dodge', stat='identity') +
    labs(title=paste(site),
         x ='Day',
         y ='Number of Events') +
    theme(legend.position=ifelse((s==5 | s==11), 'right', 'none'))
  plots_events_per_site_daily[[s]] = p
  
  # Operations by hour
  pdata_hourly = data.frame()
  for (p in 1:4) {
    hourly_ops = c()
    for (hour in hours) {
      hourly_ops = c(hourly_ops, sum(events_site$Hour==hour & events_site$MonitoringPeriod==p))
    }
    pdata_hourly = rbind(pdata_hourly, data.frame(
      Hour=hours,
      NumOps=hourly_ops,
      TimePeriod=get_time_period_for_hours(0:23),
      MonitoringPeriod=p
    ))
  }
  pdata_hourly$MonitoringPeriod = as.factor(pdata_hourly$MonitoringPeriod)
  
  p2 = ggplot(data=pdata_hourly, aes(fill=MonitoringPeriod, color=MonitoringPeriod, group=MonitoringPeriod, x=Hour, y=NumOps)) +
    geom_bar(position='dodge', stat='identity') +
    labs(title=paste(site),
         x ='Hour',
         y ='Number of Events') +
    theme(legend.position=ifelse((s==5 | s==11), 'right', 'none'))
  plots_events_per_site_hourly[[s]] = p2
}

(plots_events_per_site_daily[[1]] + plots_events_per_site_daily[[2]] + plots_events_per_site_daily[[3]]) / (plots_events_per_site_daily[[4]] + plots_events_per_site_daily[[5]] + plot_spacer())

(plots_events_per_site_daily[[6]] + plots_events_per_site_daily[[7]] + plots_events_per_site_daily[[8]]) / (plots_events_per_site_daily[[9]] + plots_events_per_site_daily[[10]] + plots_events_per_site_daily[[11]])

(plots_events_per_site_hourly[[1]] + plots_events_per_site_hourly[[2]] + plots_events_per_site_hourly[[3]]) / (plots_events_per_site_hourly[[4]] + plots_events_per_site_hourly[[5]] + plot_spacer())

(plots_events_per_site_hourly[[6]] + plots_events_per_site_hourly[[7]] + plots_events_per_site_hourly[[8]]) / (plots_events_per_site_hourly[[9]] + plots_events_per_site_hourly[[10]] + plots_events_per_site_hourly[[11]])
