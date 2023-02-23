source('global.R')
source('data/metrics/metrics.R')
library(mapview)
library(dplyr)
library(scales)
library(patchwork)

data_sites = get_data_sites()

data_metrics = get_data_metrics()
data_metrics$Field = unlist(lapply(data_metrics$ID, get_field_name_for_ID))

# NOTE: only look at Navy data for now
data_metrics = na.omit(data_metrics)
data_metrics = data_metrics[data_metrics$Org=='NAVY',]

data_ops = get_data_ops()

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

# TODO: change 0 ops days line to dashed style? or do levels with lines and ops with bars?
p_mean_ops_field_day = ggplot() +
  geom_line(data=df_mean_ops_lden_day, aes(x=Day, y=Ops, group=Field, color=Field), stat='identity') +
  labs(title='Average flight operations per day',
       subtitle=paste('Ault:', sum(mean_ops_day_ault), 'ops per week\nCoup:', sum(mean_ops_day_coup), 'ops per week'),
       x='',
       y='Operations')
p_mean_lden_field_day = ggplot() +
  geom_bar(data=df_mean_ops_lden_day, aes(x=Day, y=Lden, fill=Field), stat='identity', position='dodge', alpha=0.9) +
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

inactive_site_date_metrics = rbind(
  data_metrics[data_metrics$Field=='Ault' & !(data_metrics$Day %in% days_ault_active),],
  data_metrics[data_metrics$Field=='Coup' & !(data_metrics$Day %in% days_coup_active),]
)

p_lden_site = ggplot(active_site_date_metrics, aes(x=reorder(Name, Lden, FUN=median), y=Lden, fill=Field)) + 
  geom_violin(alpha=0.9) +
  # geom_boxplot(alpha=0.9) +
  stat_summary(fun='median', geom='point') +
  labs(title='Lden per site, active days of operation', x ='Site', y ='Lden (dBA)') +
  geom_hline(yintercept=l_hudfaa, linetype='dotted', size=0.7, colour='red') +
  geom_hline(yintercept=l_epa, linetype='dotted', size=0.7, colour='red') +
  geom_hline(yintercept=l_who, linetype='dotted', size=0.7, colour='red') +
  coord_flip()
print(p_lden_site)

# TODO: compare with inactive site dates

# Annoyance for days of average and maximum activity, all sites ----------------
# What is the risk of high annoyance at these sites based on exposure-response relationships?

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

regression_ISO = function(Lden) {
  return(-9.199 * 10^-5 * (Lden - 40)^3 + 3.932 * 10^-2 * (Lden - 40)^2 + 0.294 * (Lden - 40))
}
# Yokoshima et al 2021
regression_japan = function(Lden) {
  return(-68.080 + 1.838 * Lden + 0.006 * Lden^2) # (R^2 = 0.994)
}

# Median
median_lden = tapply(active_site_date_metrics$Lden, active_site_date_metrics$ID, median)
median_lden_HA = data.frame(
  Stat='Median',
  Lden=sort(median_lden),
  HA=regression_WHO(sort(median_lden))
)

# Max
max_lden = tapply(active_site_date_metrics$Lden, active_site_date_metrics$ID, max)
max_lden_HA = data.frame(
  Stat='Max',
  Lden=sort(max_lden),
  HA=regression_WHO(sort(max_lden))
)

combo = rbind(median_lden_HA, max_lden_HA)

p_ha = ggplot() +
  labs(x='Lden (dBA)', y='%HA', title='WHO - Percent Highly Annoyed') +
  xlim(40, max(combo$Lden)+5) +
  ylim(0, max(combo$HA)+5) +
  
  stat_function(fun=regression_WHO, xlim=c(40,75)) +
  stat_function(fun=regression_WHO, xlim=c(75,100), linetype='dashed') +
  stat_function(fun=regression_MO, xlim=c(40,75), color='red') +
  stat_function(fun=regression_MO, xlim=c(75,200), color='red', linetype='dashed') +
  # stat_function(fun=regression_ISO, colour= 'green') +
  geom_ribbon(
    data=data.frame(
      Lden=  c(40,    45,    50,    55,    60,    65),
      HAmin= c(8.1,  22.2,  33.7,  45.9, 58.8, 69.0),
      HAmax= c(21.0, 30.1,  42.4,  54.6, 66.7, 82.0)
    ), aes(x=Lden,ymin=HAmin,ymax=HAmax),
    fill='blue', alpha=0.2) +
  stat_function(fun=regression_japan, xlim=c(40,65), colour= 'purple') +
  stat_function(fun=regression_japan, xlim=c(65,100), colour= 'purple', linetype='dashed') +
  geom_point(
    data=combo,
    aes(x=Lden, y=HA, color=factor(Stat)),
    # label=rownames(median_lden_lnight_HSD),
    size=3
  ) +
  # scale_x_continuous(name='Lden (dBA)', limits=c(40,75), oob=rescale_none) +
  geom_hline(yintercept=100, linetype='dashed')
print(p_ha)

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

# TODO: histogram per period instead?
p_mean_ops_field_hour = ggplot() +
  geom_line(data=df_mean_ops_hour, aes(x=Hour, y=Ops, group=Field, color=Field), stat='identity') +
  geom_vline(xintercept='22', linetype='dotted', size=0.7, colour='red') + # Lnight
  labs(title='Average active day flight operations per hour',
       subtitle=paste('Ault:', round(sum(mean_ops_hour_ault)), 'ops per day\nCoup:', round(sum(mean_ops_hour_coup)), 'ops per day'),
       x='',
       y='Operations')
p_mean_leq_field_hour = ggplot() +
  geom_bar(data=df_mean_ops_hour, aes(x=Hour, y=Leq, fill=Field), stat='identity', position='dodge', alpha=0.9) +
  scale_y_continuous(name='Leq (dBA)', limits=c(40,90), oob=rescale_none) +
  labs(title='Average hourly Leq',
       subtitle=paste('Sentinel sites', get_site_name_for_ID(sentinel_sites[1]),
                      'and', get_site_name_for_ID(sentinel_sites[2])))
print(p_mean_ops_field_hour / p_mean_leq_field_hour)

# Lnight per site and airfield on days of activity -----------------------------

# WHO Guideline 'strong' recommendation. Evidence for a relevant absolute risk of sleep disturbance related to night noise exposure from aircraft at 40 dB Lnight was rated moderate quality. 
l_hsd_who = 40

p_lnight_site = ggplot(active_site_date_metrics, aes(x=reorder(Name, Lden_Lnight, FUN=median), y=Lden_Lnight, fill=Field)) + 
  # geom_boxplot(alpha=0.9) +
  geom_violin(alpha=0.9) +
  # geom_boxplot(alpha=0.9) +
  stat_summary(fun='median', geom='point') +
  labs(title='Lnight per site, active days of operation', x ='Site', y ='Lnight (dBA)') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_hline(yintercept=l_hsd_who, linetype='dotted', size=0.7, colour='red') +
  coord_flip()
print(p_lnight_site)
# TODO: active nights of operation, not full 24 hour periods?

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

p_hsd = ggplot() +
  labs(x='Lnight (dBA)', y='%HSD', title='WHO - Percent Highly Annoyed') +
  xlim(40, 70) +
  ylim(0, 60) +
  stat_function(fun=eq_hsd_awakenings,       color='pink') +
  stat_function(fun=eq_hsd_fallingasleep,    color='magenta') +
  stat_function(fun=eq_hsd_sleepdisturbance, color='purple') +
  stat_function(fun=eq_hsd_combinedestimate) +
  # geom_point(
  #   data=combo,
  #   aes(x=Lden, y=HA, color=factor(Stat)),
  #   # label=rownames(median_lden_lnight_HSD),
  #   size=3
  # ) +
  # geom_hline(yintercept=100, linetype='dashed') +
  geom_vline(xintercept=65, linetype='dashed')
print(p_hsd)

# TODO: Sleep disturbance for events of average SEL, all sites ------
# TODO