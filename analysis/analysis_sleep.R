# Sleep disturbance

source('global.R')

data_metrics = get_data_metrics()

# Maximum Leq hourly heatmap per day --------------------------------------------
daily_levels = na.omit(data_metrics[data_metrics$Org=='NAVY', ])

data_hour_day_levels = data.frame()
for (hour in 0:23) {
  leq_hr = paste('Leq', formatC(hour, width=2, flag='0'), sep='')
  result = tapply(daily_levels[,leq_hr], INDEX=daily_levels$Day, FUN=max)
  result = data.frame(Hour=hour, Day=names(result), Leq=result)
  rownames(result) = c()
  data_hour_day_levels = rbind(data_hour_day_levels, result)
}
data_hour_day_levels$Day = factor(data_hour_day_levels$Day, levels=days)

p = ggplot(data_hour_day_levels[order(as.numeric(data_hour_day_levels$Day)),], aes(x=Hour, y=Day, fill=Leq)) +
  geom_tile() +
  scale_fill_viridis(option='A') +
  labs(title='Max Leq heatmap across all Navy sites', x='Hour', y='Day') +
  scale_x_continuous('Hour', labels = as.character(0:23), breaks = 0:23) +
  coord_flip()
print(p)

# Lnight per site --------------------------------------------------------------

site_date_lnights = na.omit(data_metrics[,c('Org', 'Date', 'Name', 'ID', 'Lden_Lnight')])
p_lnights = ggplot(site_date_lnights, aes(x=reorder(ID, Lden_Lnight, FUN=median), y=Lden_Lnight, fill=Org)) + 
  geom_boxplot(alpha=0.3) +
  labs(title='Lnight per site (Whidbey Island area)', x ='', y ='Lden_Lnight (dBA)') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_hline(yintercept=40, linetype='dotted', colour='red') # WHO Guideline 'strong' recommendation. Evidence for a relevant absolute risk of sleep disturbance related to night noise exposure from aircraft at 40 dB Lnight was rated moderate quality. 
print(p_lnights)

# Exposure response ------------------------------------------------------------

# The percent predicted to be highly sleep-disturbed in relation to exposure to aircraft traffic noise. Based on the WHO regression model in the systematic review specified as %HSD = 16.79–0.9293 × Lnight + 0.0198 × Lnight^2
# TODO: account for confidence intervals
regression_HSD = function(Lnight) {
  return(16.79 - 0.9293 * Lnight + 0.0198 * Lnight^2)
}

# # Plot HSD (highly sleep-disturbed)
# ggplot(data.frame(
#   lnight=c(40,    45,    50,    55,    60,    65),
#   tmin=  c(4.72,  6.95,  9.87,  13.57, 18.15, 23.65),
#   tmax=  c(17.81, 23.08, 29.60, 37.41, 46.36, 56.05)
# )) +
#   stat_function(fun=regression_HSD) +
#   geom_ribbon(aes(x=lnight,ymin=tmin,ymax=tmax),fill='blue',alpha=0.2)

# Median
median_lden_lnight = tapply(data_metrics[!is.na(data_metrics$Lden_Lnight),'Lden_Lnight'], data_metrics[!is.na(data_metrics$Lden_Lnight),'ID'], median)
median_lden_lnight_HSD = data.frame(
  Stat='Median',
  Lnight=sort(median_lden_lnight),
  HSD=regression_HSD(sort(median_lden_lnight)))
median_lden_lnight_HSD$ID = rownames(median_lden_lnight_HSD)

# Max
max_lden_lnight = tapply(data_metrics[!is.na(data_metrics$Lden_Lnight),'Lden_Lnight'], data_metrics[!is.na(data_metrics$Lden_Lnight),'ID'], max)
max_lden_lnight_HSD = data.frame(
  Stat='Max',
  Lnight=sort(max_lden_lnight),
  HSD=regression_HSD(sort(max_lden_lnight)))
max_lden_lnight_HSD$ID = rownames(max_lden_lnight_HSD)

combo = rbind(median_lden_lnight_HSD, max_lden_lnight_HSD)

# TODO: combine with ribbon plot above
# TODO: look into alternate representation via odds ratio ("forest") relationship of 1.94 (95% CI: 1.61-2.33) per 10 dB increase in noise

p_hsd = ggplot() +
  labs(x='Lnight (dBA)', y='%HSD', title='WHO - Percent Highly Sleep Disturbed') +
  geom_ribbon(
    data=data.frame(
      lnight=c(40,    45,    50,    55,    60,    65),
      tmin=  c(4.72,  6.95,  9.87,  13.57, 18.15, 23.65),
      tmax=  c(17.81, 23.08, 29.60, 37.41, 46.36, 56.05)
    ), aes(x=lnight,ymin=tmin,ymax=tmax),
    fill='blue', alpha=0.2) +
  stat_function(fun=regression_HSD) +
  geom_point(
    data=combo,
    aes(x=Lnight, y=HSD, color=factor(Stat)),
    # label=rownames(median_lden_lnight_HSD),
    )
print(p_hsd)
