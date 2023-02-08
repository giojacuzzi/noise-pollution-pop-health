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
  
  period = substring(basename(file), 9, 9) # Monitoring period
  data_period = read.csv(file)
  data_period$Time       = as.POSIXct(data_period$Time)
  data_period$Hour       = strftime(data_period$Time, format='%H')
  data_period$TimePeriod = get_time_period_for_hours(data_period$Hour)
  data_period$Day        = factor(weekdays(data_period$Time, abbreviate=T), levels=days)
  data_period$Period     = period
  
  data_ault = rbind(data_ault, data_period)
  
  # Operations by hour
  hourly_ops = c()
  for (hour in hours) hourly_ops = c(hourly_ops, sum(data_period$Hour==hour))
  
  pdata_hourly = data.frame(
    Hour=hours,
    NumOps=hourly_ops,
    TimePeriod=get_time_period_for_hours(0:23)
  )

  p_ault_hourly = ggplot(data=pdata_hourly, aes(x=Hour, y=NumOps, fill=TimePeriod)) +
    geom_bar(stat='identity') +
    labs(title='Recorded Ops by Hour - Ault Field',
         subtitle=paste('Week period', period, 'Total:', sum(hourly_ops)),
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
    labs(title='Recorded Ops by Weekday - Ault Field',
         subtitle=paste('Week period', period, 'Total:', sum(daily_ops)),
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

data_coup = read.csv('data/flight_ops/output/coup/Coupeville Ops.csv')
data_coup$Time       = as.POSIXct(data_coup$Time)
data_coup$Hour       = strftime(data_coup$Time, format='%H')
data_coup$TimePeriod = get_time_period_for_hours(data_coup$Hour)
data_coup$Day        = factor(weekdays(data_coup$Time, abbreviate=T), levels=days)
data_coup$Month      = months(data_coup$Time, abbreviate=T)
data_coup$Period     = unlist(lapply(data_coup$Month, FUN=function(x){ switch(x, 'Dec'=1, 'Mar'=2, 'Apr'=2, 'Jun'=3, 'Aug'=4) }))

plots_coup_hourly = list()
plots_coup_daily  = list()

for (period in 1:4) {
  
  data_period = data_coup[data_coup$Period==period,]

  # Operations by hour
  hourly_ops = c()
  for (hour in hours) hourly_ops = c(hourly_ops, sum(data_period$Hour==hour))
  
  pdata_hourly = data.frame(
    Hour=hours,
    NumOps=hourly_ops,
    TimePeriod=get_time_period_for_hours(0:23)
  )
  
  p_coup_hourly = ggplot(data=pdata_hourly, aes(x=Hour, y=NumOps, fill=TimePeriod)) +
    geom_bar(stat='identity') +
    labs(title='Recorded Ops by Hour - OLF Coupeville',
         subtitle=paste('Week period', period, 'Total:', sum(hourly_ops)),
         x ='Hour',
         y ='Number of Operations') +
    scale_fill_manual(values=c('#150e5c',
                               '#abb5ff',
                               '#9c2a4b'))
  plots_coup_hourly[[period]] = p_coup_hourly
  
  # Operations by weekday
  daily_ops = c()
  for (day in days) daily_ops = c(daily_ops, sum(data_period$Day==day))
  
  pdata_daily = data.frame(
    Day=factor(days, levels=days),
    NumOps=daily_ops
  )
  
  p_coup_daily = ggplot(data=pdata_daily[order(pdata_daily$Day), ], aes(x=Day, y=NumOps)) +
    geom_bar(stat='identity') +
    labs(title='Recorded Ops by Weekday - OLF Coupeville',
         subtitle=paste('Week period', period, 'Total:', sum(daily_ops)),
         x ='Day',
         y ='Number of Operations')
  plots_coup_daily[[period]] = p_coup_daily
}

# OLF Coupeville plotting ------------------------------------------------------

# Operations by hour
(plots_coup_hourly[[1]] + plots_coup_hourly[[2]]) / (plots_coup_hourly[[3]] + plots_coup_hourly[[4]])
# Operations by weekday
(plots_coup_daily[[1]] + plots_coup_daily[[2]]) / (plots_coup_daily[[3]] + plots_coup_daily[[4]])

