source('global.R')
source('data/metrics/metrics.R')
library(mapview)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)

data_sites = get_data_sites()

data_metrics = get_data_metrics()
data_metrics$Field = unlist(lapply(data_metrics$ID, get_field_name_for_ID))

# NOTE: only look at Navy data for now
data_metrics = na.omit(data_metrics)
data_metrics = data_metrics[data_metrics$Org=='NAVY',]

data_ops = get_data_ops()
data_events = get_data_events()

# Monitoring site map  ---------------------------------------------------------

measured_data_sites = data_sites[data_sites$ID %in% unique(data_metrics$ID),]

mapviewOptions(legend.pos='bottomright')
mapview(
  measured_data_sites,
  xcol='Longitude', ycol='Latitude', zcol='Org',
  crs=4269, grid=FALSE, legend=TRUE,
  col.regions=c('navy', 'green3', 'darkturquoise'),
  # col.regions=c('sienna','gold','blue','green3','salmon','darkturquoise','purple3'),
  layer.name = 'Organization'
)

# Average Lden and total/field ops sentinel Whidbey sites all days -------------
# When is the navy active throughout the week, and how does it affect overall levels?

# TODO: show distribution as well

sentinel_sites = c(
  '8B_SG', # Ault Field
  '24A_B'  # OLF Coupeville
)
sentinel_metrics_ault = data_metrics[data_metrics$ID==sentinel_sites[1],]
sentinel_metrics_coup = data_metrics[data_metrics$ID==sentinel_sites[2],]

energyavg_lden_day_ault = tapply(X=sentinel_metrics_ault$Lden, INDEX=sentinel_metrics_ault$Day, FUN=energyavg)
energyavg_lden_day_coup = tapply(X=sentinel_metrics_coup$Lden, INDEX=sentinel_metrics_coup$Day, FUN=energyavg)

# Average ops by field and day of week
ops_per_ault_date = summary(data_ops[data_ops$Field=='Ault',]$Date)
ops_ault_df = data.frame(
  Date=as.POSIXct(names(ops_per_ault_date), tz='UTC'),
  Day=factor(weekdays(as.POSIXct(names(ops_per_ault_date), tz='UTC'), abbreviate=T), levels=days),
  Ops=ops_per_ault_date
)
mean_ops_day_ault = tapply(ops_ault_df$Ops, ops_ault_df$Day, mean)
ops_per_coup_date = summary(data_ops[data_ops$Field=='Coup',]$Date)
ops_coup_df = data.frame(
  Date=as.POSIXct(names(ops_per_coup_date), tz='UTC'),
  Day=factor(weekdays(as.POSIXct(names(ops_per_coup_date), tz='UTC'), abbreviate=T), levels=days),
  Ops=ops_per_coup_date
)
mean_ops_day_coup = tapply(ops_coup_df$Ops, ops_coup_df$Day, mean)

df_mean_ops_lden_day = data.frame(
  Day   = factor(rep(days,2), levels=days),
  Field = factor(c(rep('Ault',7), rep('Coup',7))),
  Ops   = c(mean_ops_day_ault, mean_ops_day_coup),
  Lden  = c(energyavg_lden_day_ault, energyavg_lden_day_coup)
)

p_mean_ops_field_day = ggplot() +
  geom_bar(data=df_mean_ops_lden_day, aes(x=Day, y=Ops, fill=Field), stat='identity', position='dodge', alpha=0.9) +
  labs(title='Average flight operations per day',
       subtitle=paste('Ault:', round(sum(mean_ops_day_ault)), 'ops per week\nCoup:', round(sum(mean_ops_day_coup)), 'ops per week'),
       x='',
       y='Operations')
p_mean_lden_field_day = ggplot() +
  geom_line(data=df_mean_ops_lden_day, aes(x=Day, y=Lden, group=Field, color=Field), stat='identity') +
  scale_y_continuous(name='Lden (dBA)', limits=c(50,90), oob=rescale_none) +
  labs(title='Average daily Lden',
       subtitle=paste('Sentinel sites', get_site_name_for_ID(sentinel_sites[1]),
                      'and', get_site_name_for_ID(sentinel_sites[2])))
print(p_mean_ops_field_day / p_mean_lden_field_day)

# Lden per site and airfield on days of activity -------------------------------
# During days of activity, what are overall levels throughout the region?

days_ault_active = df_mean_ops_lden_day[df_mean_ops_lden_day$Field=='Ault' & df_mean_ops_lden_day$Ops > 0,]$Day
days_coup_active = df_mean_ops_lden_day[df_mean_ops_lden_day$Field=='Coup' & df_mean_ops_lden_day$Ops > 0,]$Day

# HUD and Federal Aviation Regulation Part 150 incompatible for
# residential land use
l_hudfaa = 65
# EPA recommended outdoor ambient noise level
l_epa = 55
# WHO Guideline 'strong' recommendation. Evidence for a relevant absolute risk
# of annoyance at 45 dB Lden was rated moderate quality.
l_who = 45

active_site_date_metrics = rbind(
  data_metrics[data_metrics$Field=='Ault' & data_metrics$Day %in% days_ault_active,],
  data_metrics[data_metrics$Field=='Coup' & data_metrics$Day %in% days_coup_active,]
)
active_site_date_metrics$Activity='Active'

inactive_site_date_metrics = rbind(
  data_metrics[data_metrics$Field=='Ault' & !(data_metrics$Day %in% days_ault_active),],
  data_metrics[data_metrics$Field=='Coup' & !(data_metrics$Day %in% days_coup_active),]
)
inactive_site_date_metrics$Activity='Inactive'

combined_data_metrics = rbind(inactive_site_date_metrics, active_site_date_metrics)
combined_data_metrics$Activity = factor(combined_data_metrics$Activity)
combined_data_metrics$Field = factor(combined_data_metrics$Field)
combined_data_metrics = combined_data_metrics[with(combined_data_metrics, order(Activity)), ]

p_lden_site_detail = ggplot(combined_data_metrics) +
  labs(title='Lden per site, all days', x ='Site', y ='Lden (dBA)') +
  geom_violin(aes(x=reorder(Name, Lden, FUN=median), y=Lden, color=Field), alpha=1.0) +
  stat_summary(mapping=aes(x=Name, y=Lden), fun='median', geom='crossbar', width=0.5, fatten=1.0) +
  geom_dotplot(aes(x=reorder(Name, Lden, FUN=median), y=Lden, fill=Activity), binaxis='y', binwidth=1, stackdir='center', stackratio=1.2, dotsize=0.5, alpha=0.5, color=NA) +
  scale_fill_manual(values=c('Active'='red', 'Inactive'='darkgray')) +
  coord_flip()
print(p_lden_site_detail)

p_lden_site_active = ggplot(combined_data_metrics[combined_data_metrics$Field=='Coup',], aes(x=reorder(Name, Lden, FUN=median), y=Lden, fill=Activity)) + 
  geom_violin(alpha=0.9) +
  # geom_boxplot(alpha=0.9) +
  stat_summary(fun='median', geom='crossbar', width=0.1, fatten=1.0) +
  labs(title='Lden per Coupeville site, active vs inactive days of operation', x ='Site', y ='Lden (dBA)') +
  geom_hline(yintercept=l_hudfaa, linetype='dotted', size=0.7, colour='red') +
  geom_hline(yintercept=l_epa, linetype='dotted', size=0.7, colour='red') +
  geom_hline(yintercept=l_who, linetype='dotted', size=0.7, colour='red') +
  coord_flip()
print(p_lden_site_active)

p_lden_site_all = ggplot(combined_data_metrics, aes(x=reorder(Name, Lden, FUN=median), y=Lden, fill=Field)) + 
  geom_violin(alpha=0.9) +
  # geom_boxplot(alpha=0.9) +
  stat_summary(mapping=aes(y=Lden), fun='median', geom='crossbar', width=0.5, fatten=1.0) +
  stat_summary(mapping=aes(y=Lden), fun='energyavg', geom='point') +
  labs(title='Lden per site, all days', x ='Site', y ='Lden (dBA)') +
  geom_hline(yintercept=l_hudfaa, linetype='dotted', size=0.7, colour='red') +
  geom_hline(yintercept=l_epa, linetype='dotted', size=0.7, colour='red') +
  geom_hline(yintercept=l_who, linetype='dotted', size=0.7, colour='red') +
  coord_flip()
print(p_lden_site_all)

# TODO: compare with inactive site dates

# Annoyance for days of average and maximum activity, all sites ----------------
# What is the risk of high annoyance at these sites based on exposure-response relationships?

# TODO: Annoyance on a longer, "proper" timescale? Energy averaged across the four monitoring periods?

# See ISO 1996-1 2016 Annex E/F and Lct

# The percent predicted to be highly annoyed in relation to exposure to aircraft traffic noise. Based on the WHO regression equation %HA = −50.9693 + 1.0168 × Lden + 0.0072 × Lden^2 derived from the systematic review (Guski et al., 2017).
# TODO: should be only defined for Lden [40, 75]
regression_WHO = function(Lden) {
  return(-50.9693 + 1.0168 * Lden + 0.0072 * Lden^2)
}

# Miedema and Oudshoorn 2001
regression_MO = function(Lden) {
  return(-9.199 * 10^-5 * (Lden - 42)^3 + 3.932 * 10^-2 * (Lden - 42)^2 + 0.2939 * (Lden - 42))
}

regression_ISO_Fidell = function(Lden) {
  # Includes ISO recommended Lct of 71.3 dB to implement a 7 dB adjustment
  # 1996-1 2016 / Fidell et al 2011
  Lct = 71.3
  return(
    100 * exp(1)^(-(1/(10^(0.1*(Lden-Lct+4.7))))^0.3)
  )
}
regression_ISO_Miedema = function(Lden) {
  # Includes ISO recommended 7 dB adjustment
  return(-9.199 * 10^-5 * (Lden - 40)^3 + 3.932 * 10^-2 * (Lden - 40)^2 + 0.294 * (Lden - 40))
}

# Yokoshima et al 2021
regression_japan = function(Lden) {
  return(-68.080 + 1.838 * Lden + 0.006 * Lden^2) # (R^2 = 0.994)
}

# Median
median_lden = tapply(data_metrics$Lden, data_metrics$ID, median)
median_lden_HA = data.frame(
  Stat='Median',
  Lden=sort(median_lden),
  HA_WHO=regression_WHO(sort(median_lden)),
  HA_JAPAN=regression_japan(sort(median_lden)),
  HA_MO=regression_MO(sort(median_lden))
)

# Energy average
energyavg_lden = tapply(data_metrics$Lden, data_metrics$ID, energyavg)
energyavg_lden_HA = data.frame(
  Stat='Energy Average',
  Lden=sort(energyavg_lden),
  HA_WHO=regression_WHO(sort(energyavg_lden)),
  HA_JAPAN=regression_japan(sort(energyavg_lden)),
  HA_MO=regression_MO(sort(energyavg_lden))
)

# NOTE: Time scale for ERFs is long-term, typically one year, so single-date maximum Ldens are not appropriate

combo = rbind(median_lden_HA, energyavg_lden_HA)

p_ha = ggplot() +
  labs(title='Percent population highly annoyed per site, all dates') +
  stat_function(fun=regression_WHO, xlim=c(40,75), size=.7) +
  stat_function(fun=regression_WHO, xlim=c(75,100), linetype='dashed') +
  # stat_function(fun=regression_ISO_Fidell, xlim=c(40,80), size=.7, color='green') +
  # stat_function(fun=regression_ISO_Fidell, xlim=c(80,200), color='green', linetype='dashed') +
  stat_function(fun=regression_ISO_Miedema, xlim=c(40,76), size=.7, color='green') +
  stat_function(fun=regression_ISO_Miedema, xlim=c(76, 200), color='green', linetype='dashed') +
  geom_ribbon(data=data.frame(
    Lden=  c(43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76),
    HAmin= c(0.3,0.4,0.4,0.5,0.6,0.7,0.9,1.0,1.2,1.4,1.7,1.9,2.2,2.6,3.0,3.4,3.9,4.4,5.0,5.7,6.4,7.2,8.1,9.0,10.0,11.1,12.3,13.6,15.0,16.4,18.0,19.6,21.3,23.1),
    HAmax= c(33.5,35.7,38.0,40.3,42.7,45.1,47.5,49.9,52.3,54.7,57.1,59.5,61.8,64.1,66.3,68.5,70.6,72.7,74.7,76.6,78.4,80.1,81.8,83.4,84.8,86.2,87.5,88.7,89.9,90.9,91.9,92.7,93.6,94.3)
  ), aes(x=Lden,ymin=HAmin,ymax=HAmax), fill='green', alpha=0.2) +
  stat_function(fun=regression_MO, xlim=c(40,75), size=.7, color='red') +
  stat_function(fun=regression_MO, xlim=c(75,200), color='red', linetype='dashed') +
  geom_ribbon(data=data.frame(
    Lden=  c(40,    45,    50,    55,    60,    65),
    HAmin= c(8.1,  22.2,  33.7,  45.9, 58.8, 69.0),
    HAmax= c(21.0, 30.1,  42.4,  54.6, 66.7, 82.0)
  ), aes(x=Lden,ymin=HAmin,ymax=HAmax), fill='blue', alpha=0.2) +
  stat_function(fun=regression_japan, xlim=c(40,65), size=.7, color= 'purple') +
  stat_function(fun=regression_japan, xlim=c(65,100), color= 'purple', linetype='dashed') +
  geom_point(data=combo, aes(x=Lden, y=HA_WHO,   color=factor(Stat)), size=2, alpha=0.7) +
  geom_point(data=combo, aes(x=Lden, y=HA_JAPAN, color=factor(Stat)), size=2, alpha=0.7) +
  geom_point(data=combo, aes(x=Lden, y=HA_MO,    color=factor(Stat)), size=2, alpha=0.7) +
  scale_x_continuous(name='Lden (dBA)', limits=c(45,85), oob=rescale_none) +
  scale_y_continuous(name='%HA', n.breaks=9, limits=c(0,110), oob=rescale_none) +
  geom_hline(yintercept=100, linetype='dotted') +
  labs(color='Site Lden')
print(p_ha)

energyavg_lden_HA$Name = sapply(rownames(energyavg_lden_HA), get_site_name_for_ID)
energyavg_lden_HA_long = pivot_longer(energyavg_lden_HA[,c('HA_WHO', 'HA_JAPAN', 'HA_MO', 'Name')], cols=c('HA_WHO', 'HA_JAPAN', 'HA_MO'), names_to='ERF', values_to='HA')

p_ha_site = ggplot() +
  geom_bar(data=energyavg_lden_HA_long, aes(x=reorder(Name, HA), y=HA, fill=ERF), stat='identity', position='dodge') +
  labs(title='Percent population highly annoyed per site, all dates', x ='Site', y ='%HA') +
  coord_flip()
print(p_ha_site)

# Maximum Leq hourly heatmap per day -------------------------------------------
data_hour_day_levels = data.frame()
for (hour in 0:23) {
  leq_hr = paste('Leq', formatC(hour, width=2, flag='0'), sep='')
  result = tapply(data_metrics[,leq_hr], INDEX=data_metrics$Day, FUN=max)
  result = data.frame(Hour=hour, Day=names(result), Leq=result)
  rownames(result) = c()
  data_hour_day_levels = rbind(data_hour_day_levels, result)
}
data_hour_day_levels$Day = factor(data_hour_day_levels$Day, levels=days)

p_heatmap = ggplot(data_hour_day_levels[order(as.numeric(data_hour_day_levels$Day)),], aes(x=Hour, y=Day, fill=Leq)) +
  geom_tile() +
  scale_fill_viridis(option='A') +
  labs(title='Max Leq heatmap across all Navy sites', x='Hour', y='Day') +
  scale_x_continuous('Hour', labels = as.character(0:23), breaks = 0:23) +
  coord_flip()
print(p_heatmap)

# Average active day Leq and ops per hour --------------------------------------
# TODO: show distribution as well, some days are much worse and late at night

ops_hour_ault = c()
for (hour in hours) ops_hour_ault = c(ops_hour_ault, sum(data_ops[data_ops$Field=='Ault',]$Hour==hour))
mean_ops_hour_ault = ops_hour_ault / length(unique(data_ops[data_ops$Field=='Ault',]$Date))
ops_hour_coup = c()
for (hour in hours) ops_hour_coup = c(ops_hour_coup, sum(data_ops[data_ops$Field=='Coup',]$Hour==hour))
mean_ops_hour_coup = ops_hour_coup / length(unique(data_ops[data_ops$Field=='Coup',]$Date))

active_ault_hour_metrics = subset(active_site_date_metrics[active_site_date_metrics$ID==sentinel_sites[1],], select=Leq00:Leq23)
energyavg_leq_hour_ault = sapply(active_ault_hour_metrics, energyavg)
active_coup_hour_metrics = subset(active_site_date_metrics[active_site_date_metrics$ID==sentinel_sites[2],], select=Leq00:Leq23)
energyavg_leq_hour_coup = sapply(active_coup_hour_metrics, energyavg)

df_mean_ops_hour = data.frame(
  Hour  = factor(rep(hours,2), levels=hours),
  Field = factor(c(rep('Ault',24), rep('Coup',24))),
  Ops   = c(mean_ops_hour_ault, mean_ops_hour_coup),
  Leq   = c(energyavg_leq_hour_ault, energyavg_leq_hour_coup)
)

p_mean_ops_field_hour = ggplot() +
  geom_bar(data=df_mean_ops_hour, aes(x=Hour, y=Ops, fill=Field), stat='identity', position='dodge', alpha=0.9) +
  labs(title='Average active day flight operations per hour',
       subtitle=paste('Ault:', round(sum(mean_ops_hour_ault)), 'ops per day\nCoup:', round(sum(mean_ops_hour_coup)), 'ops per day'),
       x='',
       y='Operations')
p_mean_leq_field_hour = ggplot() +
  geom_line(data=df_mean_ops_hour, aes(x=Hour, y=Leq, group=Field, color=Field), stat='identity') +
  scale_y_continuous(name='Leq (dBA)', limits=c(40,90), oob=rescale_none) +
  geom_rect(aes(xmin='00', xmax = '07', ymin = -Inf, ymax = Inf), fill = 'blue', alpha = 0.09) +
  geom_rect(aes(xmin='19', xmax = '22', ymin = -Inf, ymax = Inf), fill = 'purple', alpha = 0.06) +
  geom_rect(aes(xmin='22', xmax = '23', ymin = -Inf, ymax = Inf), fill = 'blue', alpha = 0.09) +
  labs(title='Average active day Leq per hour',
       subtitle=paste('Sentinel sites', get_site_name_for_ID(sentinel_sites[1]),
                      'and', get_site_name_for_ID(sentinel_sites[2])))
print(p_mean_ops_field_hour / p_mean_leq_field_hour)

# Lnight per site and airfield on days of activity -----------------------------

# WHO Guideline 'strong' recommendation. Evidence for a relevant absolute risk of sleep disturbance related to night noise exposure from aircraft at 40 dB Lnight was rated moderate quality. 
l_hsd_who = 40

nights_ault_active = unique(data_ops[data_ops$Field=='Ault' & data_ops$DEN=='Night','Date'])
nights_coup_active = unique(data_ops[data_ops$Field=='Coup' & data_ops$DEN=='Night','Date'])

active_night_site_date_metrics = rbind(
  data_metrics[data_metrics$Field=='Ault' & factor(format(data_metrics$Date, format_date)) %in% nights_ault_active,],
  data_metrics[data_metrics$Field=='Coup' & factor(format(data_metrics$Date, format_date)) %in% nights_coup_active,]
)
active_night_site_date_metrics$Activity='Active'

inactive_night_site_date_metrics = rbind(
  data_metrics[data_metrics$Field=='Ault' & !(factor(format(data_metrics$Date, format_date)) %in% nights_ault_active),],
  data_metrics[data_metrics$Field=='Coup' & !(factor(format(data_metrics$Date, format_date)) %in% nights_coup_active),]
)
inactive_night_site_date_metrics$Activity='Inactive'

combined_night_data_metrics = rbind(inactive_night_site_date_metrics, active_night_site_date_metrics)
combined_night_data_metrics$Activity = factor(combined_night_data_metrics$Activity)
combined_night_data_metrics$Field = factor(combined_night_data_metrics$Field)
combined_night_data_metrics = combined_night_data_metrics[with(combined_night_data_metrics, order(Activity)), ]

p_lnight_site_detail = ggplot(combined_night_data_metrics) +
  labs(title='Lnight per site, all nights', x ='Site', y ='Lnight (dBA)') +
  geom_violin(aes(x=reorder(Name, Lden_Lnight, FUN=median), y=Lden_Lnight, color=Field), alpha=1.0) +
  stat_summary(mapping=aes(x=Name, y=Lden_Lnight), fun='median', geom='crossbar', width=0.5, fatten=1.0) +
  geom_dotplot(aes(x=reorder(Name, Lden_Lnight, FUN=median), y=Lden_Lnight, fill=Activity), binaxis='y', binwidth=1, stackdir='center', stackratio=1.2, dotsize=0.5, alpha=0.5, color=NA) +
  scale_fill_manual(values=c('Active'='red', 'Inactive'='darkgray')) +
  coord_flip()
print(p_lnight_site_detail)

p_lnight_site_active = ggplot(combined_night_data_metrics, aes(x=reorder(Name, Lden_Lnight, FUN=median), y=Lden_Lnight, fill=Activity)) + 
  geom_violin(alpha=0.9) +
  # geom_boxplot(alpha=0.9) +
  stat_summary(fun='median', geom='crossbar', width=0.1, fatten=1.0) +
  labs(title='Lnight per site, active vs inactive nights of operation', x ='Site', y ='Lnight (dBA)') +
  geom_hline(yintercept=l_hsd_who, linetype='dotted', size=0.7, colour='red') +
  coord_flip()
print(p_lnight_site_active)

p_lnight_site_all = ggplot(combined_data_metrics, aes(x=reorder(Name, Lden_Lnight, FUN=median), y=Lden_Lnight, fill=Field)) + 
  geom_violin(alpha=0.9) +
  # geom_boxplot(alpha=0.9) +
  stat_summary(mapping=aes(y=Lden_Lnight), fun='median', geom='crossbar', width=0.5, fatten=1.0) +
  stat_summary(mapping=aes(y=Lden_Lnight), fun='energyavg', geom='point') +
  labs(title='Lnight per site, all days', x ='Site', y ='Lnight (dBA)') +
  geom_hline(yintercept=l_hsd_who, linetype='dotted', size=0.7, colour='red') +
  coord_flip()
print(p_lnight_site_all)

# TODO: compare to inactive site dates

# TODO: Sleep disturbance for nights of average and maximum Lnight, all sites ------
# What is the risk of high sleep disturbance at these sites based on exposure-response relationships?

# TODO: Military-specific / low-frequency / onset / aircraft dB penalty adjustment?

# Smith et al 2022 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9272916/
eq_hsd_awakenings = function(Lnight) {
  return(0.03132*(Lnight)^2 - 1.80203*(Lnight) + 31.28079)
}
eq_hsd_fallingasleep = function(Lnight) {
  return(0.02204*(Lnight)^2 - 0.86230*(Lnight) + 12.42449)
}
eq_hsd_sleepdisturbance = function(Lnight) {
  return(0.02664*(Lnight)^2 - 1.17389*(Lnight) + 16.46165)
}
eq_hsd_combinedestimate = function(Lnight) {
  return(0.02502*(Lnight)^2 - 1.12624*(Lnight) + 17.07421)
}

# NOTE: Time scale for ERFs is long-term, typically one year, so single-date maximum Lnights are not appropriate. "Equivalent noise levels are often used in surveys and epidemiologic studies as long-term average exposure metrics, and are therefore also often found in legislative and policy contexts. For example, the Night Noise Guidelines for Europe of the World Health Organization (WHO) define effects of nocturnal noise based on annual average outdoor Lnight ranges. The value of equivalent noise levels in describing the effects of noise on sleep is more limited, as different noise scenarios may calculate to the same equivalent noise level, but differ substantially in their sleep disturbing properties. There is general agreement that the number and acoustical properties of single noise events better reflect the actual degree of nocturnal sleep disturbance in a single night. It is thus questionable whether Lnight can be used as the only indicator for predicting the effects of noise on sleep and the consequences of noise-induced sleep disturbance, or whether supplemental noise indicators are needed

# Median
median_lnight = tapply(data_metrics$Lden_Lnight, data_metrics$ID, median)
median_lnight_HSD = data.frame(
  Stat='Median',
  Lnight=sort(median_lnight),
  HSD_smith=eq_hsd_combinedestimate(sort(median_lnight))
)

# Energy average
energyavg_lnight = tapply(data_metrics$Lden_Lnight, data_metrics$ID, energyavg)
energyavg_lnight_HSD = data.frame(
  Stat='Energy Average',
  Lnight=sort(energyavg_lnight),
  HSD_smith=eq_hsd_combinedestimate(sort(energyavg_lnight))
)

# NOTE: Time scale for ERFs is long-term, typically one year, so single-date maximum Ldens are not appropriate
combo = rbind(median_lnight_HSD, energyavg_lnight_HSD)

p_hsd = ggplot() +
  labs(title='Probability of high sleep disturbance per site, all dates') +
  stat_function(fun=eq_hsd_combinedestimate, xlim=c(40,65), size=.7) +
  stat_function(fun=eq_hsd_combinedestimate, xlim=c(65,80), linetype='dashed') +
  # geom_ribbon(data=data.frame(
  #   Lden=  c(40,    45,    50,    55,    60,    65),
  #   HAmin= c(8.1,  22.2,  33.7,  45.9, 58.8, 69.0),
  #   HAmax= c(21.0, 30.1,  42.4,  54.6, 66.7, 82.0)
  # ), aes(x=Lden,ymin=HAmin,ymax=HAmax), fill='blue', alpha=0.2) +
  # stat_function(fun=regression_japan, xlim=c(40,65), size=.7, color= 'purple') +
  # stat_function(fun=regression_japan, xlim=c(65,100), color= 'purple', linetype='dashed') +
  geom_point(data=combo, aes(x=Lnight, y=HSD_smith, color=factor(Stat)), size=2, alpha=0.7) +
  scale_x_continuous(name='Lnight (dBA)', limits=c(40,80), oob=rescale_none) +
  scale_y_continuous(name='%HSD', n.breaks=9, limits=c(0,90), oob=rescale_none) +
  labs(color='Site Lnight')
print(p_hsd)

energyavg_lnight_HSD$Name = sapply(rownames(energyavg_lnight_HSD), get_site_name_for_ID)
p_hsd_site = ggplot() +
  geom_bar(data=energyavg_lnight_HSD, aes(x=reorder(Name, HSD_smith), y=HSD_smith), stat='identity') +
  labs(title='Probability of high sleep disturbance', x ='Site', y ='%HSD') +
  coord_flip()
print(p_hsd_site)

# Num events above 60 dB -------------------------------------------------------
# Stacked per-airfield barplots of sentinel site mean noise event count per hour across periods grouped by loudness (+ number of operations at the airfield on second y axis?)

factor_breaks = c(0,40,50,60,70,80,90,1000)
factor_lables = c('<40', '40-50', '50-60','60-70','70-80','80-90','90+')

for (site in sentinel_sites) {
  events_site = data_events[data_events$SiteID==site,]
  ops_field = data_ops[data_ops$Field==get_field_name_for_ID(site),]
  events_site$Range_LAeq_Lmax = cut(events_site$LAeq_Lmax, breaks=factor_breaks, right=F)
  
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
    labs(title=paste('Mean noise event Lmax vs flight operations -', data_sites[data_sites$ID==site,'Region']),
         subtitle=paste('Site', site, '- average', sum(pops_daily$Ops), 'operations per week'),
         x ='Day',
         fill='Range (dBA)') +
    geom_point(data=pops_daily, aes(x=Day, y=Ops), size=2, color='black') +
    geom_line(data=pops_daily, aes(x=Day, y=Ops), group=1, size=1, color='black') +
    scale_y_continuous(name='Noise events', sec.axis=sec_axis(trans=~.*1, name='Flight operations'))
  print(p)
}

for (site in sentinel_sites) {
  events_site = data_events[data_events$SiteID==site,]
  ops_field = data_ops[data_ops$Field==get_field_name_for_ID(site),]
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
    labs(title=paste('Mean noise event Lmax vs flight operations -', data_sites[data_sites$ID==site,'Region']),
         subtitle=paste('Site', site, '- average', sum(pops_hourly$Ops), 'operations per week'),
         x ='Hour',
         fill='Range (dBA)') +
    geom_point(data=pops_hourly, aes(x=Hour, y=Ops), size=2, color='black') +
    geom_line(data=pops_hourly, aes(x=Hour, y=Ops), group=1, size=1, color='black') +
    scale_y_continuous(name='Noise events', sec.axis=sec_axis(trans=~.*1, name='Flight operations'))
  print(p)
}

# NOTE: ASA/ANSI TR S12.9 PART 6 withdrawn in 2018

# TODO: Sleep disturbance for events of average SEL, all sites ------
# TODO


# Hearing loss ----------------------

occupational_standards = read.csv('analysis/OshaNiosh.csv')

# OSHA action level
# https://www.osha.gov/laws-regs/regulations/standardnumber/1910/1910.95AppA#:~:text=(2)%20The%20eight%2Dhour,to%20the%20measured%20sound%20level
unique(occupational_standards[occupational_standards$OshaTWA>=85,'ID'])

# NIOSH recommended exposure limit
# https://www.cdc.gov/niosh/docs/98-126/pdfs/98-126.pdf?id=10.26616/NIOSHPUB98126
unique(occupational_standards[occupational_standards$NioshTWA>=85,'ID'])

# FAA Hearing Conservation Program action level trigger
# https://www.faa.gov/documentLibrary/media/Order/Order_3900.66A.pdf
# "The AL or the TWA exposure which requires program inclusion is 82 dBA, or a dose of 50 percent. FS employees exposed to this level for 30 days or more per year require inclusion in the HCP."
unique(occupational_standards[occupational_standards$NioshTWA>=82,'ID'])

# According to ISO 1999, exposure to environmental and leisure-time noise with LAeq,24h values < 70 dB(A) does not cause hearing impairment in the large majority of people (> 95%). In other words, an exposure limit of >70 dBA LAeq over a 24 hour period from environmental and leisure noise can pose a risk of hearing impairment.
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1637786/pdf/envhper00310-0128.pdf
# The EPA identifies a 24-hour exposure level of 70 decibels as the level of environmental noise which will prevent any measurable hearing loss over a lifetime.
# https://www.epa.gov/archive/epa/aboutepa/epa-identifies-noise-levels-affecting-health-and-welfare.html#:~:text=The%20document%20identifies%20a%2024,hearing%20loss%20over%20a%20lifetime
unique(data_metrics[data_metrics$Leq>=70,'ID'])

# Direct physiological effects related to potential health impairments and risks are found, as a rule, only where maximal sound levels are above ca 115 d B(A) Criteria for the risk of inner ear damage due to high peak noise levels (> 115 d B)
# https://drive.google.com/drive/u/0/search?q=Low-altitude%20overflights%20of%20fighters%20the%20risk%20of%20hearing%20loss
# Noise-induced hearing loss is generally from exposures to higher noise frequencies ranging from 3,000 to 6,000 Hz
# https://drive.google.com/file/d/1WZX8iRGYmG4wTG41XTzZzUyjHAkpPr2L/view
unique(data_metrics[data_metrics$Lmax>=115,'ID'])



# Recommended max onset rate of 60 dB / sec
# https://drive.google.com/drive/u/0/search?q=Low-altitude%20overflights%20of%20fighters%20the%20risk%20of%20hearing%20loss
