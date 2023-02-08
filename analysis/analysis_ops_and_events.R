# Noise event quantity vs day of week barplot per-period for Ault Field and Coupeville (see “Aircraft Noise Event Database”)
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

plots_events_per_site = list()
for (s in 1:nrow(sites)) {
  site = sites[s,'ID']
  events_site = events[events$SiteID==site,]
  events_site$MonitoringPeriod = as.factor(events_site$MonitoringPeriod)
  # mean_events = nrow(events_site)/4
  
  # p = ggplot(events_site, aes(x=LAeq_Lmax, fill=MonitoringPeriod)) +
  #   geom_histogram(binwidth=5, boundary=-2.5, position='identity', alpha = 0.5) +
  #   xlim(25, 125) + ylim(0,1500) +
  #   labs(title=paste0(site, ' (', sites[s,'Name'], ')'),
  #        subtitle=paste('Mean events per week:', mean_events)) +
  #   labs(x =ifelse((s==4 | s==9),'LAeq_Lmax',''),
  #        y =ifelse((s==4 | s==9),'Number of events','')) +
  #   theme(legend.position=ifelse((s==5 | s==11), 'right', 'none'))
  
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
    labs(title=paste('Noise Events by Weekday -', site),
         # subtitle=paste('Week period', period, 'Total:', sum(daily_ops)),
         x ='Day',
         y ='Number of Events') +
    theme(legend.position=ifelse((s==5 | s==11), 'right', 'none'))
  plots_events_per_site[[s]] = p
}

(plots_events_per_site[[1]] + plots_events_per_site[[2]] + plots_events_per_site[[3]]) / (plots_events_per_site[[4]] + plots_events_per_site[[5]] + plot_spacer())

(plots_events_per_site[[6]] + plots_events_per_site[[7]] + plots_events_per_site[[8]]) / (plots_events_per_site[[9]] + plots_events_per_site[[10]] + plots_events_per_site[[11]])

