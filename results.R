### Plot and map results

source('global.R')
library('ggplot2')
library('viridis')
library('mapview')

data_metrics_navy = read.csv('data/metrics/metrics_navy.csv')
data_metrics_navy = cbind(Org='NAVY', data_metrics_navy)
data_metrics_sda = read.csv('data/metrics/metrics_sda.csv')
data_metrics_sda = cbind(Org='SDA', data_metrics_sda)
data_metrics = rbind(data_metrics_navy, data_metrics_sda)

# Organization map -------------------------------------------------------------
mapviewOptions(legend.pos='bottomright')
mapview(
  data_sites,
  xcol='Longitude', ycol='Latitude', zcol='Org',
  crs=4269, grid=FALSE, legend=TRUE,
  col.regions=c('sienna','gold','blue','green3','salmon','darkturquoise','purple3'),
  layer.name = 'Organization'
)

# Day-night average means and maximums -----------------------------------------
mean_dnl     = tapply(data_metrics[!is.na(data_metrics$Ldn),'Ldn'], data_metrics[!is.na(data_metrics$Ldn),'ID'], mean)
mean_denl    = tapply(data_metrics[!is.na(data_metrics$Lden),'Lden'], data_metrics[!is.na(data_metrics$Lden),'ID'], mean)
mean_day     = tapply(data_metrics[!is.na(data_metrics$Lden_Lday),'Lden_Lday'], data_metrics[!is.na(data_metrics$Lden_Lday),'ID'], mean)
mean_evening = tapply(data_metrics[!is.na(data_metrics$Lden_Levening),'Lden_Levening'], data_metrics[!is.na(data_metrics$Lden_Levening),'ID'], mean)
mean_night   = tapply(data_metrics[!is.na(data_metrics$Lden_Lnight),'Lden_Lnight'], data_metrics[!is.na(data_metrics$Lden_Lnight),'ID'], mean)

max_dnl     = tapply(data_metrics[!is.na(data_metrics$Ldn),'Ldn'], data_metrics[!is.na(data_metrics$Ldn),'ID'], max)
max_denl    = tapply(data_metrics[!is.na(data_metrics$Lden),'Lden'], data_metrics[!is.na(data_metrics$Lden),'ID'], max)
max_day     = tapply(data_metrics[!is.na(data_metrics$Lden_Lday),'Lden_Lday'], data_metrics[!is.na(data_metrics$Lden_Lday),'ID'], max)
max_evening = tapply(data_metrics[!is.na(data_metrics$Lden_Levening),'Lden_Levening'], data_metrics[!is.na(data_metrics$Lden_Levening),'ID'], max)
max_night   = tapply(data_metrics[!is.na(data_metrics$Lden_Lnight),'Lden_Lnight'], data_metrics[!is.na(data_metrics$Lden_Lnight),'ID'], max)

# Ldn mean map -----------------------------------------------------------------
mapview(
  merge(data_sites, data.frame(dB=c(t(mean_dnl)), ID=rownames(mean_dnl)), all=TRUE),
  xcol='Longitude', ycol='Latitude', zcol='dB',
  cex='dB', crs=4269, grid=FALSE, legend=TRUE,
  layer.name = 'Mean Ldn (dBA)'
)


# Ldn max map ------------------------------------------------------------------
mapview(
  merge(data_sites, data.frame(dB=c(t(max_dnl)), ID=rownames(max_dnl)), all=TRUE),
  xcol='Longitude', ycol='Latitude', zcol='dB',
  cex='dB', crs=4269, grid=FALSE, legend=TRUE,
  layer.name = 'Max Ldn (dBA)'
)

# Day-night average grouped barplot --------------------------------------------
dnl_denl = data.frame(mean_dnl, mean_denl, max_dnl, max_denl)
ggplot(
  data.frame(dB = c(t(dnl_denl[,])),
             id = rep(rownames(dnl_denl),each=4),
             metric = c('Mean Ldn','Mean Lden','Max Ldn','Max Lden')),
  aes(fill=metric, y=dB, x=id)) + 
  geom_bar(position='dodge', stat='identity') +
  geom_hline(yintercept=85, linetype='dotted', col='red') + # OSHA 8-hour exposure
  theme_minimal() + 
  labs(x='Site', y='dB', title='Day-night average means and maximums') +
  scale_fill_manual('', values=viridis(4))

# Level maximums and peaks -----------------------------------------------------
data_metrics_max = data_metrics
data_metrics_max[is.na(data_metrics_max)] = 0
max_Lmax = tapply(data_metrics_max$Lmax, data_metrics_max$ID, max)
max_LCpeak = tapply(data_metrics_max$LCpeak, data_metrics_max$ID, max)
l_maxpeak = data.frame(max_Lmax, max_LCpeak)
ggplot(
  data.frame(dB = c(t(l_maxpeak[,])),
             id = rep(rownames(l_maxpeak),each=2),
             metric = c('Lmax (dBA)','Lpeak (dBC)')),
  aes(fill=metric, y=dB, x=id)) + 
  geom_bar(position='dodge', stat='identity') +
  theme_minimal() + 
  labs(x='Site', y='dB', title='Level maximums and peaks') +
  scale_fill_manual('',values=viridis(4))


# Dates by Ldn -----------------------------------------------------------------
dates_by_ldn = data_metrics[with(data_metrics,order(-Ldn)),]
dates_by_ldn[1:20]

# Health Impacts ---------------------------------------------------------------

# The percent predicted to be highly annoyed in relation to exposure to aircraft traffic noise. Based on the WHO regression equation %HA = −50.9693 + 1.0168 × Lden + 0.0072 × Lden^2 derived from the systematic review (Guski et al., 2017).
# TODO: should be only defined for Lden [40, 75]
regression_HA = function(Lden) {
  return(-50.9693 + 1.0168 * Lden + 0.0072 * Lden^2)
}

# Plot HA (highly annoyed)
mean_lden_HA = data.frame(Lden=sort(mean_denl), HA=regression_HA(sort(mean_denl)))
max_lden_HA = data.frame(Lden=sort(max_denl), HA=regression_HA(sort(max_denl)))
# Mean
ggplot(mean_lden_HA, aes(x=Lden, y=HA, label=rownames(mean_lden_HA))) +
  labs(x='Lden (dB)', y='%HA', title='Percent Highly Annoyed (Mean)') +
  geom_point(col='blue') + geom_text(hjust=0, vjust=1.5, col='blue') +
  stat_function(fun=regression_HA)
# Max
ggplot(max_lden_HA, aes(x=Lden, y=HA, label=rownames(max_lden_HA))) +
  labs(x='Lden (dB)', y='%HA', title='Percent Highly Annoyed (Max)') +
  geom_point(col='red') + geom_text(hjust=0, vjust=1.5, col='red') +
  geom_hline(yintercept=100, linetype='dashed') + # 100% HA
  stat_function(fun=regression_HA)

# The percent predicted to be highly sleep-disturbed in relation to exposure to aircraft traffic noise. Based on the WHO regression model in the systematic review specified as %HSD = 16.79–0.9293 × Lnight + 0.0198 × Lnight^2
# TODO: account for confidence intervals
regression_HSA = function(Lnight) {
  return(-50.9693 + 1.0168 * Lnight + 0.0072 * Lnight^2)
}

# Plot HSA (highly sleep-disturbed)
mean_lden_lnight_HSA = data.frame(Lden=sort(mean_night), HA=regression_HSA(sort(mean_night)))
max_lden_lnight_HSA = data.frame(Lden=sort(max_night), HA=regression_HSA(sort(max_night)))
# Mean
ggplot(mean_lden_lnight_HSA, aes(x=Lden, y=HA, label=rownames(mean_lden_lnight_HSA))) +
  labs(x='Lnight (dB)', y='%HSA', title='Percent Highly Sleep Disturbed (Mean)') +
  geom_point(col='blue') + geom_text(hjust=0, vjust=1.5, col='blue') +
  stat_function(fun=regression_HA)
# Max
ggplot(max_lden_lnight_HSA, aes(x=Lden, y=HA, label=rownames(max_lden_lnight_HSA))) +
  labs(x='Lnight (dB)', y='%HSA', title='Percent Highly Sleep Disturbed (Max)') +
  geom_point(col='red') + geom_text(hjust=0, vjust=1.5, col='red') +
  geom_hline(yintercept=100, linetype='dashed') + # 100% HSA
  stat_function(fun=regression_HA)
