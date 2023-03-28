## Averages
# Ldn, Lden

source('global.R')
source('data/metrics/metrics.R')

data_sites   = get_data_sites()
data_metrics = get_data_metrics()
data_metrics$Field = unlist(lapply(data_metrics$ID, get_field_name_for_ID))

library(mapview)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)
library(ggpmisc)

# NOTE: only look at Navy data for now
navy_metrics = na.omit(data_metrics)
navy_metrics = navy_metrics[navy_metrics$Org=='NAVY',]

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

lden_metrics = data_metrics[!is.na(data_metrics$Lden),]

# Energy average Ldn
# Compare with real-time measured DNL from navy report
avg_ldn = round(tapply(combined_data_metrics$Ldn, combined_data_metrics$ID, energyavg),1)
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
# NOTE: difference for Lopez (5B_SG) and PT (33_SG) is significant, potentially due to higher ambient noise from coastal wind, watercraft, and (in the case of PT) city activity. Note that the difference in Lden between these two sites during active and inactive days (below) is minimal, indicating a negligible effect of aircraft noise events on overall noise levels during this monitoring period. Therefore, we may choose to forgo including them in the health analysis.
data.frame(
  ID=ldn_comparison$ID,
  Difference=(ldn_comparison$Ldn - ldn_comparison$NavyDNL)
)
# Explanation for using continuous Lden for Navy sites
# - We do not have the data or tools necessary to classify noise events of Navy sites. Other sites (JGL, NPS, and SDA) had in-person operators validating the presence of noise events due to aircraft operations
# - Aircraft operations produce the dominant noise events in the area, and their influence on overall level values far outweighs that of any transient noise*
# - Lden values are within +/- 3 dB of Navy reported values for Navy sites (excluding Lopez and PT)
# * This may not be the case for sites far from the airfields, specifically Lopez 5B_SG and Port Townsend 33_SG


# Energy average Lden
round(tapply(lden_metrics$Lden, lden_metrics$ID, energyavg),1)

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
