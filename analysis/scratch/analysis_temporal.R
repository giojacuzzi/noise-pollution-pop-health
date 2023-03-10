# Lden per day, per season, per airfield

library(ggplot2)
sites = read.csv('data/sites/sites.csv')
metrics = read.csv('data/metrics/output/metrics_NAVY.csv')

sites_ault = sites[sites$Org=='NAVY' & sites$Region=='Ault Field', 'ID']
sites_coup = sites[sites$Org=='NAVY' & sites$Region=='OLF Coupeville', 'ID']

metrics_ault = metrics[metrics$ID %in% sites_ault,]
metrics_coup = metrics[metrics$ID %in% sites_coup,]

#-------------------------------------------------------------------------------
# Lden vs day of week histogram per-period for both fields (navy data only)

get_daily_levels = function(metrics) {
  day_abbr = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
  daily_levels = na.omit(metrics)
  daily_levels = cbind(daily_levels,
                            Day=weekdays(as.POSIXct(daily_levels$Date, tz='UTC'),abbreviate=T))
  daily_levels$Day = factor(daily_levels$Day, levels=day_abbr)
  
  daily_levels = cbind(daily_levels,
                            Month=months(as.POSIXct(daily_levels$Date, tz='UTC'), abbreviate=T))
  daily_levels$Month = factor(daily_levels$Month, levels=month.abb)
  daily_levels = cbind(daily_levels, Period=daily_levels$Month)
  levels(daily_levels$Period)[levels(daily_levels$Period)=='Mar'] <- 'Mar/Apr'
  levels(daily_levels$Period)[levels(daily_levels$Period)=='Apr'] <- 'Mar/Apr'
  return(daily_levels)
}

levels_daily_ault = get_daily_levels(metrics_ault)
levels_daily_coup = get_daily_levels(metrics_coup)

# Lden per day - Ault Field
p_ault_levels_daily = ggplot(levels_daily_ault[order(levels_daily_ault$Day), ], aes(x=Day, y=Lden)) + 
  geom_boxplot(alpha=0.4) +
  labs(title='Lden per day - Ault Field Sites, Total', x ='Day', y ='Lden (dBA)') +
  geom_hline(yintercept=65, linetype='dotted', colour='red') # HUD / FAA
print(p_ault_levels_daily)

p_ault_levels_daily_period = ggplot(levels_daily_ault[order(levels_daily_ault$Day), ], aes(x=Day, y=Lden, fill=Period)) + 
  geom_boxplot(alpha=0.4) +
  labs(title='Lden per day - Ault Field Sites, Seasonal', x ='Day', y ='Lden (dBA)') +
  geom_hline(yintercept=65, linetype='dotted', colour='red') # HUD / FAA
print(p_ault_levels_daily_period)

# Lden per day - OLF Coupeville
p_coup_levels_daily = ggplot(levels_daily_coup[order(levels_daily_coup$Day), ], aes(x=Day, y=Lden)) + 
  geom_boxplot(alpha=0.4) +
  labs(title='Lden per day - OLF Coupeville Sites, Total', x ='Day', y ='Lden (dBA)') +
  geom_hline(yintercept=65, linetype='dotted', colour='red') # HUD / FAA
print(p_coup_levels_daily)

p_coup_levels_daily = ggplot(levels_daily_coup[order(levels_daily_coup$Day), ], aes(x=Day, y=Lden, fill=Period)) + 
  geom_boxplot(alpha=0.4) +
  labs(title='Lden per day - OLF Coupeville Sites, Seasonal', x ='Day', y ='Lden (dBA)') +
  geom_hline(yintercept=65, linetype='dotted', colour='red') # HUD / FAA
print(p_coup_levels_daily)
