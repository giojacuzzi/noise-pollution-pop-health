# Stacked per-airfield barplots of sentinel site mean noise event count per hour across periods grouped by loudness (+ number of operations at the airfield on second y axis?)
library(ggplot2)
library(viridis)
theme_set(theme_minimal())

days  = c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')

sites = read.csv('data/sites/sites.csv')
sites = sites[sites$Org=='NAVY' & (sites$Region=='Ault Field' | sites$Region=='OLF Coupeville'), ]
events = read.csv('data/events/output/events.csv')
events$MonitoringPeriod = as.factor(events$MonitoringPeriod)
events$StartTime        = as.POSIXct(events$StartTime)
events$Hour             = as.factor(strftime(events$StartTime, format='%H'))
events$Day              = factor(weekdays(events$StartTime, abbreviate=T), levels=days)

# Ault Field: 9B_SG (NASWI Gate)
# OLF Coupeville: 24A_B (Reuble Farm)
sites_to_plot = c('9B_SG', '24A_B')

for (site in sites_to_plot) {
  events_site = events[events$SiteID==site,]
  
  factor_breaks = c(0,40,50,60,70,80,90,1000)
  factor_lables = c('<40', '40-50', '50-60','60-70','70-80','80-90','90+')
  events_site$Range_LAeq_Lmax = cut(events_site$LAeq_Lmax, breaks=factor_breaks, right=F)

  num_events_per_range_hour = tapply(X=events_site$Range_LAeq_Lmax, INDEX=events_site$Hour, FUN=summary)
  # Average across 4 periods
  mean_events_per_range_hour = lapply(num_events_per_range_hour, function(x){x/4})
  
  pdata_hourly = data.frame()
  for (hour in names(mean_events_per_range_hour)) {
    nevnt = mean_events_per_range_hour[[hour]]
    range = names(mean_events_per_range_hour[[hour]])
    pdata_hourly = rbind(pdata_hourly, data.frame(
      Hour=hour,
      Events=nevnt,
      Range=range
    ))
  }
  pdata_hourly$Hour = as.factor(pdata_hourly$Hour)
  pdata_hourly$Range = factor(pdata_hourly$Range, labels=factor_lables)
  
  p = ggplot(data=pdata_hourly, aes(x=Hour, y=Events, group=Range, fill=Range)) +
    geom_bar(stat='identity') +
    scale_fill_viridis_d(option='magma') +
    labs(title=paste('Mean noise event Lmax -', sites[sites$ID==site,'Region']),
         subtitle=paste('Site', site, '- average', sum(pdata_hourly$Events), 'events per week'),
         x ='Hour',
         y ='Mean number of events',
         fill='LAeq_Lmax')
  print(p)

  num_events_per_range_day = tapply(X=events_site$Range_LAeq_Lmax, INDEX=events_site$Day, FUN=summary)
  # Average across 4 periods
  mean_events_per_range_day = lapply(num_events_per_range_day, function(x){x/4})
  
  pdata_daily = data.frame()
  for (day in names(mean_events_per_range_day)) {
    nevnt = mean_events_per_range_day[[day]]
    range = names(mean_events_per_range_day[[day]])
    pdata_daily = rbind(pdata_daily, data.frame(
      Day=factor(day, levels=days),
      Events=nevnt,
      Range=range
    ))
  }
  pdata_daily$Day = as.factor(pdata_daily$Day)
  pdata_daily$Range = factor(pdata_daily$Range, labels=factor_lables)
  
  p = ggplot(data=pdata_daily[order(pdata_daily$Day), ], aes(x=Day, y=Events, group=Range, fill=Range)) +
    geom_bar(stat='identity') +
    scale_fill_viridis_d(option='magma') +
    labs(title=paste('Mean noise event Lmax -', sites[sites$ID==site,'Region']),
         subtitle=paste('Site', site, '- average', sum(pdata_daily$Events), 'events per week'),
         x ='Day',
         y ='Mean number of events',
         fill='LAeq_Lmax')
  print(p)
  
}
