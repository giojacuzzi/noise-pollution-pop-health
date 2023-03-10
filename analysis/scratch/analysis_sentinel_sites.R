# Stacked per-airfield barplots of sentinel site mean noise event count per hour across periods grouped by loudness (+ number of operations at the airfield on second y axis?)
source('global.R')

library(ggplot2)
library(viridis)

sites = read.csv('data/sites/sites.csv')
sites = sites[sites$Org=='NAVY' & (sites$Region=='Ault Field' | sites$Region=='OLF Coupeville'), ]

events = read.csv('data/events/output/events.csv')
events$StartTime  = as.POSIXct(events$StartTime)
events$Hour       = strftime(events$StartTime, format='%H')
events$DEN        = get_den_period_for_hours(events$Hour)
events$Day        = factor(weekdays(events$StartTime, abbreviate=T), levels=days)
events$Period     = get_navy_monitoring_period_for_times(events$StartTime)

#### TODO: incorporate ops
ops = read.csv('data/flight_ops/output/ops.csv')
ops$Time       = as.POSIXct(ops$Time)
ops$Hour       = as.factor(strftime(ops$Time, format='%H'))
ops$DEN        = get_den_period_for_hours(ops$Hour)
ops$Day        = factor(weekdays(ops$Time, abbreviate=T), levels=days)
ops$Period     = get_navy_monitoring_period_for_times(ops$Time)

# Ault Field: 9B_SG (NASWI Gate)
# OLF Coupeville: 24A_B (Reuble Farm)
sites_to_plot = c('9B_SG', '24A_B')

for (site in sites_to_plot) {
  events_site = events[events$SiteID==site,]
  ops_field = ops[ops$Field==get_field_name_for_ID(site),]
  
  factor_breaks = c(0,40,50,60,70,80,90,1000)
  factor_lables = c('<40', '40-50', '50-60','60-70','70-80','80-90','90+')
  events_site$Range_LAeq_Lmax = cut(events_site$LAeq_Lmax, breaks=factor_breaks, right=F)

  # Average events across all 4 periods
  num_events_per_range_hour = tapply(X=events_site$Range_LAeq_Lmax, INDEX=events_site$Hour, FUN=summary)
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
  
  # Average ops across all 4 periods
  num_ops_per_hour = summary(ops_field$Hour)
  mean_ops_hour = lapply(num_ops_per_hour, function(x){x/4})
  pops_hourly = as.data.frame(as.table(unlist(mean_ops_hour)))
  names(pops_hourly) = c('Hour', 'Ops')
  
  p = ggplot() +
    geom_bar(data=pdata_hourly, aes(x=Hour, y=Events, group=Range, fill=Range), stat='identity') +
    scale_fill_viridis_d(option='magma') +
    labs(title=paste('Mean noise event Lmax vs flight operations -', sites[sites$ID==site,'Region']),
         subtitle=paste('Site', site, '- average', sum(pops_hourly$Ops), 'operations per week'),
         x ='Hour',
         fill='Range (dBA)') +
    geom_point(data=pops_hourly, aes(x=Hour, y=Ops), size=2, color='black') +
    geom_line(data=pops_hourly, aes(x=Hour, y=Ops), group=1, size=1, color='black') +
    scale_y_continuous(name='Noise events', sec.axis=sec_axis(trans=~.*1, name='Flight operations'))
  print(p)

  # Average events across 4 periods
  num_events_per_range_day = tapply(X=events_site$Range_LAeq_Lmax, INDEX=events_site$Day, FUN=summary)
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
  
  # Average ops across 4 periods
  num_ops_per_day = summary(ops_field$Day)
  mean_ops_daily = lapply(num_ops_per_day, function(x){x/4})
  pops_daily = as.data.frame(as.table(unlist(mean_ops_daily)))
  names(pops_daily) = c('Day', 'Ops')

  p = ggplot() +
    geom_bar(data=pdata_daily[order(pdata_daily$Day), ], aes(x=Day, y=Events, group=Range, fill=Range), stat='identity') +
    scale_fill_viridis_d(option='magma') +
    labs(title=paste('Mean noise event Lmax vs flight operations -', sites[sites$ID==site,'Region']),
         subtitle=paste('Site', site, '- average', sum(pops_daily$Ops), 'operations per week'),
         x ='Day',
         fill='Range (dBA)') +
    geom_point(data=pops_daily, aes(x=Day, y=Ops), size=2, color='black') +
    geom_line(data=pops_daily, aes(x=Day, y=Ops), group=1, size=1, color='black') +
    scale_y_continuous(name='Noise events', sec.axis=sec_axis(trans=~.*1, name='Flight operations'))
  print(p)
}

