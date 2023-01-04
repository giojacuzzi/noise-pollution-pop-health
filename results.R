### Plot and map results

source('global.R')
library('ggplot2')
library('viridis')
library('mapview')

data_metrics = get_data_metrics()

# NOTE: Some SDA measurements were recorded with overloaded gains (i.e. distortion) that result in erroneously high values during flybys. Here, we remove site dates with measurements exceeding 110 dB.
data_metrics = data_metrics[-which(data_metrics$Org == 'SDA' & data_metrics$Lmax > 110.0),]

# Organization map -------------------------------------------------------------
mapviewOptions(legend.pos='bottomright')
mapview(
  get_data_sites(),
  xcol='Longitude', ycol='Latitude', zcol='Org',
  crs=4269, grid=FALSE, legend=TRUE,
  col.regions=c('sienna','gold','blue','green3','salmon','darkturquoise','purple3'),
  layer.name = 'Organization'
)

# Ldn median map -----------------------------------------------------------------
median_dnl = tapply(data_metrics[!is.na(data_metrics$Ldn),'Ldn'], data_metrics[!is.na(data_metrics$Ldn),'ID'], median)

mapview(
  merge(get_data_sites(), data.frame(dB=c(t(median_dnl)), ID=rownames(median_dnl)), all=TRUE),
  xcol='Longitude', ycol='Latitude', zcol='dB',
  cex='dB', crs=4269, grid=FALSE, legend=TRUE,
  layer.name = 'Median Ldn (dBA)'
)

# Level maximums and peaks -----------------------------------------------------
# data_metrics_max = data_metrics
# data_metrics_max[is.na(data_metrics_max)] = 0
# max_Lmax = tapply(data_metrics_max$Lmax, data_metrics_max$ID, max)
# max_LCpeak = tapply(data_metrics_max$LCpeak, data_metrics_max$ID, max)
# l_maxpeak = data.frame(max_Lmax, max_LCpeak)
# ggplot(
#   data.frame(dB = c(t(l_maxpeak[,])),
#              id = rep(rownames(l_maxpeak),each=2),
#              metric = c('Lmax (dBA)','Lpeak (dBC)')),
#   aes(fill=metric, y=dB, x=id)) + 
#   geom_bar(position='dodge', stat='identity') +
#   theme_minimal() + 
#   labs(x='Site', y='dB', title='Level maximums and peaks') +
#   scale_fill_manual('',values=viridis(4))



# Plot site date Ldns -----------------------------------------------------------

site_date_ldns = na.omit(data_metrics[,c('Org', 'Date', 'Name', 'ID', 'Ldn')])

ggplot(site_date_ldns, aes(x=reorder(ID, Ldn, FUN=median), y=Ldn, fill=Org)) + 
  geom_boxplot(alpha=0.3) +
  labs(title='Day-night-average levels per site', x ='Site ID', y ='Ldn (dBA)') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_hline(yintercept=65, linetype='dotted', colour='red') + # HUD and Federal Aviation Regulation Part 150 incompatible for residential land use
  geom_hline(yintercept=55, linetype='dotted', colour='red') # EPA recommended outdoor ambient noise level

# Plot site date maximums ------------------------------------------------------
site_date_maximums = na.omit(data_metrics[,c('Org', 'Date', 'Name', 'ID', 'Lmax')])

ggplot(site_date_maximums, aes(x=reorder(ID, Lmax, FUN=median), y=Lmax, fill=Org)) + 
  geom_boxplot(alpha=0.3) +
  labs(title='Maximum levels per site', x ='Site ID', y ='Lmax (dBA)') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Plot date site hour spl ------------------------------------------------------
# source('data/load_site_date.R')
# create_splplot = function(id, date, hour) {
#   site_date_data = load_site_date(id, date)
#   start = (hour*60*60) + 1
#   end = (start + 60*60/2) - 1
#   ggplot(site_date_data[start:end,], aes(x=Time, y=Value)) +
#     geom_line() +
#     ggtitle(paste(id, date, 'hour', hour))
# }
# 
# hour = 0
# id = 'KntP'
# date = '2020-07-08'
# create_splplot(id, date, hour)

# Health Impacts ---------------------------------------------------------------

# The percent predicted to be highly annoyed in relation to exposure to aircraft traffic noise. Based on the WHO regression equation %HA = −50.9693 + 1.0168 × Lden + 0.0072 × Lden^2 derived from the systematic review (Guski et al., 2017).
# TODO: should be only defined for Lden [40, 75]
regression_HA = function(Lden) {
  return(-50.9693 + 1.0168 * Lden + 0.0072 * Lden^2)
}

# Plot HA (highly annoyed)

# Median
median_lden = tapply(data_metrics[!is.na(data_metrics$Lden),'Lden'], data_metrics[!is.na(data_metrics$Lden),'ID'], median)
median_lden_HA = data.frame(Lden=sort(median_lden), HA=regression_HA(sort(median_lden)))

ggplot(median_lden_HA, aes(x=Lden, y=HA, label=rownames(median_lden_HA))) +
  labs(x='Lden Median (dB)', y='%HA', title='Percent Highly Annoyed (Median Lden)') +
  geom_point(col='blue') + geom_text(hjust=0, vjust=1.5, col='blue') +
  stat_function(fun=regression_HA)

# Max
max_lden = tapply(data_metrics[!is.na(data_metrics$Lden),'Lden'], data_metrics[!is.na(data_metrics$Lden),'ID'], max)
max_lden_HA = data.frame(Lden=sort(max_lden), HA=regression_HA(sort(max_lden)))

ggplot(max_lden_HA, aes(x=Lden, y=HA, label=rownames(max_lden_HA))) +
  labs(x='Lden Maximum (dB)', y='%HA', title='Percent Highly Annoyed (Maximum Lden)') +
  geom_point(col='red') + geom_text(hjust=0, vjust=1.5, col='red') +
  geom_hline(yintercept=100, linetype='dashed') + # 100% HA
  stat_function(fun=regression_HA)

# The percent predicted to be highly sleep-disturbed in relation to exposure to aircraft traffic noise. Based on the WHO regression model in the systematic review specified as %HSD = 16.79–0.9293 × Lnight + 0.0198 × Lnight^2
# TODO: account for confidence intervals
regression_HSA = function(Lnight) {
  return(-50.9693 + 1.0168 * Lnight + 0.0072 * Lnight^2)
}

# Plot HSA (highly sleep-disturbed)

# Mean
median_lden_lnight = tapply(data_metrics[!is.na(data_metrics$Lden_Lnight),'Lden_Lnight'], data_metrics[!is.na(data_metrics$Lden_Lnight),'ID'], median)
median_lden_lnight_HSA = data.frame(Lden=sort(median_lden_lnight), HA=regression_HSA(sort(median_lden_lnight)))

ggplot(median_lden_lnight_HSA, aes(x=Lden, y=HA, label=rownames(median_lden_lnight_HSA))) +
  labs(x='Lnight Median (dB)', y='%HSA', title='WHO - Percent Highly Sleep Disturbed (Median Lnight)') +
  geom_point(col='blue') + geom_text(hjust=0, vjust=1.5, col='blue') +
  geom_hline(yintercept=0, linetype='dashed') + # 0% HSA
  stat_function(fun=regression_HA)

# Max
max_lden_lnight = tapply(data_metrics[!is.na(data_metrics$Lden_Lnight),'Lden_Lnight'], data_metrics[!is.na(data_metrics$Lden_Lnight),'ID'], max)
max_lden_lnight_HSA = data.frame(Lden=sort(max_lden_lnight), HA=regression_HSA(sort(max_lden_lnight)))

ggplot(max_lden_lnight_HSA, aes(x=Lden, y=HA, label=rownames(max_lden_lnight_HSA))) +
  labs(x='Lnight Maximum (dB)', y='%HSA', title='WHO - Percent Highly Sleep Disturbed (Maximum Lnight)') +
  geom_point(col='red') + geom_text(hjust=0, vjust=1.5, col='red') +
  stat_function(fun=regression_HA)

# Time Breakdowns (NAVY only) --------------------------------------------------
day_abbr = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')

# Ldn per day
daily_ldns = na.omit(data_metrics[data_metrics$Org=='NAVY', c('Org', 'Date', 'Name', 'ID', 'Ldn')])
daily_ldns = cbind(daily_ldns, Day=weekdays(as.POSIXct(daily_ldns$Date, tz='UTC'), abbreviate=T))
daily_ldns$Day = factor(daily_ldns$Day, levels=day_abbr)
daily_ldns = daily_ldns[order(daily_ldns$Day), ]

ggplot(daily_ldns, aes(x=Day, y=Ldn, fill=Org)) + 
  geom_boxplot(alpha=0.3) +
  labs(title='Ldn per day across all Navy sites', x ='Day', y ='Ldn (dBA)') +
  geom_hline(yintercept=65, linetype='dotted', colour='red') # HUD / FAA

# Lmax per day
daily_lmax = na.omit(data_metrics[data_metrics$Org=='NAVY', c('Org', 'Date', 'Name', 'ID', 'Lmax')])
daily_lmax = cbind(daily_lmax, Day=weekdays(as.POSIXct(daily_lmax$Date, tz='UTC'), abbreviate=T))
daily_lmax$Day = factor(daily_lmax$Day, levels=day_abbr)
daily_lmax = daily_lmax[order(daily_lmax$Day), ]

ggplot(daily_lmax, aes(x=Day, y=Lmax, fill=Org)) + 
  geom_boxplot(alpha=0.3) +
  labs(title='Lmax per day across all Navy sites', x ='Day', y ='Lmax (dBA)') +
  geom_hline(yintercept=65, linetype='dotted', colour='red') # HUD / FAA

# Ldn per day, per season





