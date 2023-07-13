## Averages
# Ldn, Lden

source('global.R')
source('metrics/metrics.R')

data_sites   = get_data_sites()
data_metrics = get_data_metrics()
data_metrics$Field = unlist(lapply(data_metrics$ID, get_field_name_for_ID))

navy_metrics = na.omit(data_metrics)
navy_metrics = navy_metrics[navy_metrics$Org=='NAVY',]

# Calculated Ldn vs Navy-measured DNL vs NOISEMAP model DNL --------------------

# Compare with real-time measured DNL from navy report
ldn_metrics = navy_metrics[!is.na(navy_metrics$Ldn),]
avg_ldn = round(tapply(ldn_metrics$Ldn, ldn_metrics$ID, energyavg),1)
ldn_comparison = data.frame(ID=names(avg_ldn), Ldn=avg_ldn)
ldn_comparison$NavyDNL = NA
ldn_comparison[ldn_comparison$ID=='2B_T', 'NavyDNL']   = 64.8
ldn_comparison[ldn_comparison$ID=='3A_T', 'NavyDNL']   = 62.1
ldn_comparison[ldn_comparison$ID=='5B_SG', 'NavyDNL']  = 44.1
ldn_comparison[ldn_comparison$ID=='8B_SG', 'NavyDNL']  = 70.1
ldn_comparison[ldn_comparison$ID=='9B_SG', 'NavyDNL']  = 65.9
ldn_comparison[ldn_comparison$ID=='20B_SG', 'NavyDNL'] = 71.6
ldn_comparison[ldn_comparison$ID=='24A_B', 'NavyDNL']  = 84.3
ldn_comparison[ldn_comparison$ID=='25B_T', 'NavyDNL']  = 69.9
ldn_comparison[ldn_comparison$ID=='26B_SG', 'NavyDNL'] = 74.2
ldn_comparison[ldn_comparison$ID=='27A_SG', 'NavyDNL'] = 69.2
ldn_comparison[ldn_comparison$ID=='33_SG', 'NavyDNL']  = 39.9
# Compare with real-time modeled DNL from navy report, energy-averaged over each period's modeled DNL
ldn_comparison$NavyModeledDNL = NA
ldn_comparison[ldn_comparison$ID=='2B_T', 'NavyModeledDNL']   = 67.9
ldn_comparison[ldn_comparison$ID=='3A_T', 'NavyModeledDNL']   = 63.2
ldn_comparison[ldn_comparison$ID=='5B_SG', 'NavyModeledDNL']  = 44.7
ldn_comparison[ldn_comparison$ID=='8B_SG', 'NavyModeledDNL']  = 76.6
ldn_comparison[ldn_comparison$ID=='9B_SG', 'NavyModeledDNL']  = 73.1
ldn_comparison[ldn_comparison$ID=='20B_SG', 'NavyModeledDNL'] = 79.4
ldn_comparison[ldn_comparison$ID=='24A_B', 'NavyModeledDNL']  = 85.1
ldn_comparison[ldn_comparison$ID=='25B_T', 'NavyModeledDNL']  = 67.9
ldn_comparison[ldn_comparison$ID=='26B_SG', 'NavyModeledDNL'] = 77.5
ldn_comparison[ldn_comparison$ID=='27A_SG', 'NavyModeledDNL'] = 75.2
ldn_comparison[ldn_comparison$ID=='33_SG', 'NavyModeledDNL']  = 40.8
# Average flight operations in aggregate model across all 4 navy periods
ldn_comparison$ModeledDNL = NA # taken from NOISEMAP aggregate .poi
ldn_comparison[ldn_comparison$ID=='2B_T', 'ModeledDNL']   = 70.5
ldn_comparison[ldn_comparison$ID=='3A_T', 'ModeledDNL']   = 70.6
ldn_comparison[ldn_comparison$ID=='5B_SG', 'ModeledDNL']  = 50.3
ldn_comparison[ldn_comparison$ID=='8B_SG', 'ModeledDNL']  = 78.9
ldn_comparison[ldn_comparison$ID=='9B_SG', 'ModeledDNL']  = 75.8
ldn_comparison[ldn_comparison$ID=='20B_SG', 'ModeledDNL'] = 80.6
ldn_comparison[ldn_comparison$ID=='24A_B', 'ModeledDNL']  = 86.3
ldn_comparison[ldn_comparison$ID=='25B_T', 'ModeledDNL']  = 69.8
ldn_comparison[ldn_comparison$ID=='26B_SG', 'ModeledDNL'] = 79.5
ldn_comparison[ldn_comparison$ID=='27A_SG', 'ModeledDNL'] = 76.4
ldn_comparison[ldn_comparison$ID=='33_SG', 'ModeledDNL']  = 41.6

# NOTE: difference for Lopez (5B_SG) and PT (33_SG) is significant, potentially due to higher ambient noise from coastal wind, watercraft, and (in the case of PT) city activity. Note that the difference in Lden between these two sites during active and inactive days (below) is minimal, indicating a negligible effect of aircraft noise events on overall noise levels during this monitoring period. Therefore, we may choose to forgo including them in the site-specific health analysis.
comparison = data.frame(
  ID=factor(ldn_comparison$ID),
  Ldn_PhiMeasured    = ldn_comparison$Ldn,
  DNL_PhiModeled     = ldn_comparison$ModeledDNL,
  DNL_NavyMeasured   = ldn_comparison$NavyDNL,
  DNL_NavyModeled    = ldn_comparison$NavyModeledDNL,
  D_LdnPhiMeasured_DNLNavyMeasured = (ldn_comparison$Ldn - ldn_comparison$NavyDNL),
  D_LdnPhiMeasured_DNLPhiModeled  = (ldn_comparison$Ldn - ldn_comparison$ModeledDNL),
  D_DNLNavyMeasured_DNL_PhiModeled = (ldn_comparison$NavyDNL - ldn_comparison$ModeledDNL),
  D_DNLNavyModeled_DNL_PhiModeled = (ldn_comparison$NavyModeledDNL - ldn_comparison$ModeledDNL)
)
ggplot(gather(comparison, Source, Level, Ldn_PhiMeasured:DNL_NavyModeled, factor_key=T), aes(x = reorder(ID, Level, FUN=mean), y = Level, color = Source)) +
  geom_point(shape = 1)

# Navy model energy average vs Phi model
ggplot(comparison, aes(x = DNL_NavyModeled, y = DNL_PhiModeled, color = ID)) + geom_point() + geom_abline(slope=1, intercept=0)
cor(comparison$DNL_PhiModeled, comparison$DNL_NavyModeled, method='pearson')
mean(comparison$DNL_PhiModeled - comparison$DNL_NavyModeled)

# Navy measured vs Phi model
ggplot(comparison, aes(x = DNL_NavyMeasured, y = DNL_PhiModeled, color = ID)) + geom_point() + geom_abline(slope=1, intercept=0)
cor(comparison$DNL_PhiModeled, comparison$DNL_NavyMeasured, method='pearson')
mean(comparison$DNL_PhiModeled - comparison$DNL_NavyMeasured)

# Explanation for using continuous Lden for Navy sites
# - We do not have the data or tools necessary to classify noise events of Navy sites. Other sites (JGL, NPS, and SDA) had in-person operators validating the presence of noise events due to aircraft operations
# - Aircraft operations produce the dominant noise events in the area, and their influence on overall level values far outweighs that of any transient noise*
# - Ldn values are within +/- 3 dB of Navy reported measured values for Navy sites (excluding Lopez and PT)
summary(comparison[!(comparison$ID %in% c('5B_SG', '33_SG')), 'D_LdnPhiMeasured_DNLNavyMeasured'])
# - Modeled DNL vaues are with +/- 10 dB of calculated Ldn values (excluding PT), mean absolute error 5.88 dB
summary(comparison[!(comparison$ID %in% c('5B_SG', '33_SG')), 'D_LdnPhiMeasured_DNLPhiModeled'])
# * This may not be the case for sites far from the airfields, specifically Lopez 5B_SG and Port Townsend 33_SG

# Energy average Lden
lden_metrics = data_metrics[!is.na(data_metrics$Lden),]
round(tapply(lden_metrics$Lden, lden_metrics$ID, energyavg),1)

# Lden per site ----------------------------------------------------------------
# During days of activity/inactivity, what are overall levels throughout the region?
# Dependencies: any dataset for overview, only NAVY dataset for in/activity detail

# HUD and Federal Aviation Regulation Part 150 incompatible for
# residential land use
l_hudfaa = 65
# EPA recommended outdoor ambient noise level
l_epa = 55
# WHO Guideline 'strong' recommendation. Evidence for a relevant absolute risk
# of annoyance at 45 dB Lden was rated moderate quality.
l_who = 45

# Use weekday/weekend split as marker of high/low ("active/inactive") activity
days_ault_active = c('Mon','Tue','Wed','Thu','Fri')
# For operations: df_mean_ops_lden_day[df_mean_ops_lden_day$Field=='Ault' & df_mean_ops_lden_day$Ops > 0,]$Day
days_coup_active = c('Mon','Tue','Wed','Thu','Fri')
# For operations: df_mean_ops_lden_day[df_mean_ops_lden_day$Field=='Coup' & df_mean_ops_lden_day$Ops > 0,]$Day

active_site_date_metrics = rbind(
  navy_metrics[navy_metrics$Field=='Ault' & navy_metrics$Day %in% days_ault_active,],
  navy_metrics[navy_metrics$Field=='Coup' & navy_metrics$Day %in% days_coup_active,]
)
active_site_date_metrics$Activity='Active'

inactive_site_date_metrics = rbind(
  navy_metrics[navy_metrics$Field=='Ault' & !(navy_metrics$Day %in% days_ault_active),],
  navy_metrics[navy_metrics$Field=='Coup' & !(navy_metrics$Day %in% days_coup_active),]
)
inactive_site_date_metrics$Activity='Inactive'

combined_data_metrics = rbind(inactive_site_date_metrics, active_site_date_metrics)
combined_data_metrics$Activity = factor(combined_data_metrics$Activity)
combined_data_metrics$Field = factor(combined_data_metrics$Field)
combined_data_metrics = combined_data_metrics[with(combined_data_metrics, order(Activity)), ]

# Overview
# NOTE: to organize by field, add `color=Field` to geom_boxplot and stat_summary
p_lden_site_all = ggplot() +
  geom_boxplot(data=lden_metrics, mapping=aes(x=reorder(Name, Lden, FUN=energyavg), y=Lden), outlier.size=0.9) +
  stat_summary(data=lden_metrics, mapping=aes(x=reorder(Name, Lden, FUN=energyavg), y=Lden, shape='Energy average'), fun='energyavg', geom='point', size=3, fill='white') +
  scale_shape_manual('', values=c('Energy average'=21)) +
  labs(title='Lden per site', x ='Site', y ='Lden (dBA)') +
  geom_hline(yintercept=l_hudfaa, linetype='dotted', size=0.7, colour='red') +
  geom_hline(yintercept=l_epa, linetype='dotted', size=0.7, colour='red') +
  coord_flip()
print(p_lden_site_all)
ggsave(p_lden_site_all, file=paste0(ggsave_output_path, 'lden_site.png'), width=ggsave_width, height=ggsave_height)

# Active vs inactive view
activity_colors = c('salmon','gray')
p_lden_site_active = ggplot() +
  geom_boxplot(data=combined_data_metrics, mapping=aes(x=reorder(Name, Lden, FUN=energyavg), y=Lden, fill=Activity), color='gray30', outlier.size=0.6) +
  stat_summary(data=combined_data_metrics, mapping=aes(x=reorder(Name, Lden, FUN=energyavg), y=Lden, shape='Energy average', fill=Activity), fun='energyavg', geom='point', size=3, color='gray30', position=position_dodge(width=0.75)) +
  scale_shape_manual('', values=c('Energy average'=21)) +
  scale_fill_manual(name='Flight activity', values=activity_colors) +
  labs(title='Lden per site by flight activity', subtitle='(Weekday vs weekend)', x ='Site', y ='Lden (dBA)') +
  geom_hline(yintercept=l_hudfaa, linetype='dotted', size=0.7, colour='red') +
  geom_hline(yintercept=l_epa, linetype='dotted', size=0.7, colour='red') +
  coord_flip()
print(p_lden_site_active)
ggsave(p_lden_site_active, file=paste0(ggsave_output_path, 'lden_site_activity.png'), width=ggsave_width, height=ggsave_height)

# Active vs inactive difference
tapply(active_site_date_metrics$Lden, active_site_date_metrics$Name, energyavg) - tapply(inactive_site_date_metrics$Lden, inactive_site_date_metrics$Name, energyavg)

# Lnight per site --------------------------------------------------------------
# Dependencies: any dataset for overview, only NAVY dataset for in/activity detail

# WHO Guideline 'strong' recommendation. Evidence for a relevant absolute risk of sleep disturbance related to night noise exposure from aircraft at 40 dB Lnight was rated moderate quality. 
l_hsd_who = 40

# Use weekday/weekend split as marker of high/low ("active/inactive") activity
nights_ault_active = c('Mon','Tue','Wed','Thu','Fri')
# For operations: unique(data_ops[data_ops$Field=='Ault' & data_ops$DEN=='Night','Date'])
nights_coup_active = c('Mon','Tue','Wed','Thu','Fri')
# For operations: unique(data_ops[data_ops$Field=='Coup' & data_ops$DEN=='Night','Date'])

active_night_site_date_metrics = rbind( # factor(format(data_metrics$Date, format_date))
  navy_metrics[navy_metrics$Field=='Ault' & navy_metrics$Day %in% nights_ault_active,],
  navy_metrics[navy_metrics$Field=='Coup' & navy_metrics$Day %in% nights_coup_active,]
)
active_night_site_date_metrics$Activity='Active'

inactive_night_site_date_metrics = rbind(
  navy_metrics[navy_metrics$Field=='Ault' & !(navy_metrics$Day %in% nights_ault_active),],
  navy_metrics[navy_metrics$Field=='Coup' & !(navy_metrics$Day %in% nights_coup_active),]
)
inactive_night_site_date_metrics$Activity='Inactive'

combined_night_data_metrics = rbind(inactive_night_site_date_metrics, active_night_site_date_metrics)
combined_night_data_metrics$Activity = factor(combined_night_data_metrics$Activity)
combined_night_data_metrics$Field = factor(combined_night_data_metrics$Field)
combined_night_data_metrics = combined_night_data_metrics[with(combined_night_data_metrics, order(Activity)), ]

lnight_metrics = data_metrics[!is.na(data_metrics$Lden_Lnight),]

# Overview
p_lnight_site_all = ggplot() + 
  geom_boxplot(data=combined_night_data_metrics, mapping=aes(x=reorder(Name, Lden_Lnight, FUN=energyavg), y=Lden_Lnight), outlier.size=0.9) +
  stat_summary(data=combined_night_data_metrics, mapping=aes(x=reorder(Name, Lden_Lnight, FUN=energyavg), y=Lden_Lnight, shape='Energy average'), fun='energyavg', geom='point', size=3, fill='white') +
  scale_shape_manual('', values=c('Energy average'=21)) +
  labs(title='Lnight per site', x ='Site', y ='Lnight (dBA)', color='Airfield') +
  # geom_hline(yintercept=l_hsd_who, linetype='dotted', size=0.7, colour='red') +
  coord_flip()
print(p_lnight_site_all)
ggsave(p_lnight_site_all, file=paste0(ggsave_output_path, 'lnight_site.png'), width=ggsave_width, height=ggsave_height)

# Active vs inactive view
activity_colors = c('salmon','gray')
p_lnight_site_active = ggplot() + 
  geom_boxplot(data=combined_night_data_metrics, mapping=aes(x=reorder(Name, Lden_Lnight, FUN=energyavg), y=Lden_Lnight, fill=Activity), color='gray30', outlier.size=0.6) +
  stat_summary(data=combined_night_data_metrics, mapping=aes(x=reorder(Name, Lden_Lnight, FUN=energyavg), y=Lden_Lnight, shape='Energy average', fill=Activity), fun='energyavg', geom='point', size=3, color='gray30', position=position_dodge(width=0.75)) +
  scale_shape_manual('', values=c('Energy average'=21)) +
  scale_fill_manual(name='Flight activity', values=activity_colors) +
  labs(title='Lnight per site by flight activity', subtitle='(Weekday vs weekend)', x ='Site', y ='Lnight (dBA)') +
  # geom_hline(yintercept=l_hsd_who, linetype='dotted', size=0.7, colour='red') +
  coord_flip()
print(p_lnight_site_active)
ggsave(p_lnight_site_active, file=paste0(ggsave_output_path, 'lnight_site_activity.png'), width=ggsave_width, height=ggsave_height)

# Active vs inactive difference
tapply(active_night_site_date_metrics$Lden_Lnight, active_night_site_date_metrics$Name, energyavg) - tapply(inactive_night_site_date_metrics$Lden_Lnight, inactive_night_site_date_metrics$Name, energyavg)
