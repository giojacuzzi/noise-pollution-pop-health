# Bar plots of flight operation counts vs hour of day / day of week (per-airfield)
source('global.R')

library(ggplot2)
library(patchwork)

data = read.csv('data/flight_ops/output/ops.csv')
data$Time       = as.POSIXct(data$Time)
data$Hour       = strftime(data$Time, format='%H')
data$DEN        = get_den_period_for_hours(data$Hour)
data$Day        = factor(weekdays(data$Time, abbreviate=T), levels=days)
data$Period     = get_navy_monitoring_period_for_times(data$Time)

# Data processing ---------------------------------------------------

plots_hourly = list()
plots_daily = list()

for (field in unique(data$Field)) {
  for (period in 1:4) {
    
    data_period = data[data$Period==period & data$Field==field,]
    
    # Operations by hour
    hourly_ops = c()
    for (hour in hours) hourly_ops = c(hourly_ops, sum(data_period$Hour==hour))
    
    pdata_hourly = data.frame(
      Hour=hours,
      NumOps=hourly_ops,
      TimePeriod=get_den_period_for_hours(0:23)
    )
    
    p_hourly = ggplot(data=pdata_hourly, aes(x=Hour, y=NumOps, fill=TimePeriod)) +
      geom_bar(stat='identity') +
      labs(title=paste0('Recorded Ops by Hour - ', field),
           subtitle=paste('Week period', period, 'Total:', sum(hourly_ops)),
           x ='Hour',
           y ='Number of Operations') +
      scale_fill_manual(values=c('#abb5ff',
                                 '#9c2a4b',
                                 '#150e5c'))
    plots_hourly = append(plots_hourly, list(p_hourly))
    
    # Operations by weekday
    daily_ops = c()
    for (day in days) daily_ops = c(daily_ops, sum(data_period$Day==day))
    
    pdata_daily = data.frame(
      Day=factor(days, levels=days),
      NumOps=daily_ops
    )
    
    p_daily = ggplot(data=pdata_daily[order(pdata_daily$Day), ], aes(x=Day, y=NumOps)) +
      geom_bar(stat='identity') +
      labs(title=paste0('Recorded Ops by Weekday - ', field),
           subtitle=paste('Week period', period, 'Total:', sum(daily_ops)),
           x ='Day',
           y ='Number of Operations')
    plots_daily = append(plots_daily, list(p_daily))
  }
}

# Plotting ---------------------------------------------------------------------

# Ault daily
print((plots_daily[[1]] + plots_daily[[2]]) / (plots_daily[[3]] + plots_daily[[4]]))
# Coup daily
print((plots_daily[[5]] + plots_daily[[6]]) / (plots_daily[[7]] + plots_daily[[8]]))

# Ault hourly
print((plots_hourly[[1]] + plots_hourly[[2]]) / (plots_hourly[[3]] + plots_hourly[[4]]))
# Coup hourly
print((plots_hourly[[5]] + plots_hourly[[6]]) / (plots_hourly[[7]] + plots_hourly[[8]]))
