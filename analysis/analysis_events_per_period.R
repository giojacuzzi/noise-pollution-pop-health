# Bar plots of noise event quantity vs hour of day / day of week (per-site, per-period)
source('global.R')

library(ggplot2)
library(patchwork)
library(stringr)

sites = read.csv('data/sites/sites.csv')
sites = sites[sites$Org=='NAVY' & (sites$Region=='Ault Field' | sites$Region=='OLF Coupeville'), ]

events = read.csv('data/events/output/events.csv')
events$StartTime  = as.POSIXct(events$StartTime)
events$Hour       = strftime(events$StartTime, format='%H')
events$DEN        = get_den_period_for_hours(events$Hour)
events$Day        = factor(weekdays(events$StartTime, abbreviate=T), levels=days)
events$Period     = get_navy_monitoring_period_for_times(events$StartTime)

plots_events_per_site_daily = list()
plots_events_per_site_hourly = list()
for (s in 1:nrow(sites)) {
  site = sites[s,'ID']
  events_site = events[events$SiteID==site,]
  events_site$Period = as.factor(events_site$Period)
  
  # Events by weekday
  pdata_daily = data.frame()
  for (p in 1:4) {
    daily_events = c()
    for (day in days) {
      daily_events = c(daily_events, sum(events_site$Day==day & events_site$Period==p))
    }
    pdata_daily = rbind(pdata_daily, data.frame(
      Day=factor(days, levels=days),
      Numevents=daily_events,
      Period=p
    ))
  }
  pdata_daily$Period = as.factor(pdata_daily$Period)
  
  p = ggplot(data=pdata_daily[order(pdata_daily$Day), ], aes(fill=Period, color=Period, group=Period, x=Day, y=Numevents)) +
    geom_bar(position='dodge', stat='identity') +
    labs(title=paste(site),
         x ='Day',
         y ='Number of Events') +
    theme(legend.position=ifelse((s==5 | s==11), 'right', 'none'))
  plots_events_per_site_daily[[s]] = p
  
  # Events by hour
  pdata_hourly = data.frame()
  for (p in 1:4) {
    hourly_events = c()
    for (hour in hours) {
      hourly_events = c(hourly_events, sum(events_site$Hour==hour & events_site$Period==p))
    }
    pdata_hourly = rbind(pdata_hourly, data.frame(
      Hour=hours,
      Numevents=hourly_events,
      Period=p
    ))
  }
  pdata_hourly$Period = as.factor(pdata_hourly$Period)
  
  p2 = ggplot(data=pdata_hourly, aes(fill=Period, color=Period, group=Period, x=Hour, y=Numevents)) +
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
