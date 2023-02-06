# Analysis of navy flight ops record from 'Flight Operations Data'
library(ggplot2)
library(patchwork)
library(stringr)

hours = str_pad(0:23, 2, pad = '0')
days  = c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')

get_time_period_for_hours = function(hours) {
  return(cut(as.numeric(hours),
             breaks=c(-1,6,18,22,23),
             labels=c('Night','Day','Evening','Night')))
}

# Ault Field data processing ---------------------------------------------------
files = list.files(path='data/flight_ops/output/ault/', pattern="*.csv", full.names=T, recursive=F)
data_ault = data.frame()
plots_ault_hourly = list()
plots_ault_daily  = list()

for (file in files) {
  
  period = substring(basename(file), 7, 9) # Monitoring period
  data_period = read.csv(file)
  data_period$LogTime           = as.POSIXct(data_period$LogTime)
  data_period$Hour              = strftime(data_period$LogTime, format='%H')
  data_period$TimePeriod        = get_time_period_for_hours(data_period$Hour)
  data_period$Day               = factor(weekdays(data_period$LogTime, abbreviate=T), levels=days)
  data_period$MonitoringPeriod  = period
  
  data_ault = rbind(data_ault, data_period)
  
  # Operations by hour
  hourly_ops = c()
  for (hour in hours) {
    hourly_ops = c(hourly_ops, sum(data_period$Hour==hour))
  }
  
  pdata_hourly = data.frame(
    Hour=hours,
    NumOps=hourly_ops,
    TimePeriod=get_time_period_for_hours(0:23)
  )

  p_ault_hourly = ggplot(data=pdata_hourly, aes(x=Hour, y=NumOps, fill=TimePeriod)) +
    geom_bar(stat='identity') +
    labs(title=paste('Recorded Flight Operations by Hour - Ault Field, Week', period),
         subtitle=paste('Total:', sum(hourly_ops)),
         x ='Hour',
         y ='Number of Operations') +
    scale_fill_manual(values=c('#150e5c',
                               '#abb5ff',
                               '#9c2a4b'))
  plots_ault_hourly[[period]] = p_ault_hourly
  
  # Operations by weekday
  daily_ops = c()
  for (day in days) daily_ops = c(daily_ops, sum(data_period$Day==day))
  
  pdata_daily = data.frame(
    Day=factor(days, levels=days),
    NumOps=daily_ops
  )
  
  p_ault_daily = ggplot(data=pdata_daily[order(pdata_daily$Day), ], aes(x=Day, y=NumOps)) +
    geom_bar(stat='identity') +
    labs(title=paste('Recorded Flight Operations by Weekday - Ault Field, Week', period),
         subtitle=paste('Total:', sum(daily_ops)),
         x ='Day',
         y ='Number of Operations')
  plots_ault_daily[[period]] = p_ault_daily
}

# Ault Field plotting ----------------------------------------------------------

# Operations by hour
(plots_ault_hourly[[1]] + plots_ault_hourly[[2]]) / (plots_ault_hourly[[3]] + plots_ault_hourly[[4]])
# Operations by weekday
(plots_ault_daily[[1]] + plots_ault_daily[[2]]) / (plots_ault_daily[[3]] + plots_ault_daily[[4]])

# OLF Coupeville data processing ---------------------------------------------------
# TODO
# files = list.files(path='data/flight_ops/output/coup/', pattern="*.csv", full.names=T, recursive=F)
# data_coop = data.frame()
# plots_coop_hourly = list()
# plots_coop_daily  = list()
