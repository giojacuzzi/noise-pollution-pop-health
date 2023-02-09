# Stacked per-airfield barplots of sentinel site mean noise event count per hour across periods grouped by loudness (+ number of operations at the airfield on second y axis?)
library(ggplot2)
library(viridis)
theme_set(theme_minimal())

sites = read.csv('data/sites/sites.csv')
sites = sites[sites$Org=='NAVY' & (sites$Region=='Ault Field' | sites$Region=='OLF Coupeville'), ]
events = read.csv('data/events/output/events.csv')
events$MonitoringPeriod = as.factor(events$MonitoringPeriod)
events$StartTime       = as.POSIXct(events$StartTime)
events$Hour       = as.factor(strftime(events$StartTime, format='%H'))

# Ault Field: 9B_SG (NASWI Gate)
# OLF Coupeville: 24A_B (Reuble Farm)
sites_to_plot = c('9B_SG', '24A_B')

for (site in sites_to_plot) {
  events_site = events[events$SiteID==site,]
  
  # TODO: cut breaks for sleep disturbance?
  events_site$Range_LAeq_Lmax = cut(events_site$LAeq_Lmax, breaks=c(0,50,70,90,200), right=F)
  
  num_events_per_period = summary(events_site$MonitoringPeriod)
  num_events_per_hour   = summary(events_site$Hour)
  num_events_per_hour_period = tapply(X=events_site$Hour, INDEX=events_site$MonitoringPeriod, FUN=summary)
  
  num_events_per_period_hour = tapply(X=events_site$MonitoringPeriod, INDEX=events_site$Hour, FUN=summary)
  mean_events_per_hour = unlist(lapply(num_events_per_period_hour, mean))
  
  num_events_per_range_hour = tapply(X=events_site$Range_LAeq_Lmax, INDEX=events_site$Hour, FUN=summary)
  mean_events_per_range_hour = lapply(num_events_per_range_hour, function(x){x/4})
  
  fixer = tapply(X=events_site$Hour, INDEX=events_site$Range_LAeq_Lmax, FUN=summary)
  mean_fixer = lapply(fixer, function(x){x/4})
  
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
  pdata_hourly$Range = factor(pdata_hourly$Range, levels =
                                c("[0,50)", "[50,70)" , "[70,90)", "[90,200)"),
                              labels = c("0-50", "50-70", "70-90", "90+"))
  
  p = ggplot(data=pdata_hourly, aes(x=Hour, y=Events, group=Range, fill=Range)) +
    geom_bar(stat='identity') +
    scale_fill_viridis_d(option='magma') +
    labs(title=paste('Mean noise event Lmax -', sites[sites$ID==site,'Region']),
         subtitle=paste('Site', site, '- average', sum(pdata_hourly$Events), 'events per week'),
         x ='Hour',
         y ='Mean number of events',
         fill='LAeq_Lmax')
  print(p)
}
