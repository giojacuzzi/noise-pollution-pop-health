## Averages
# Ldn, Lden

source('global.R')
source('metrics/metrics.R')

data_sites   = get_data_sites()
data_metrics = get_data_metrics()
data_metrics$Field = unlist(lapply(data_metrics$ID, get_field_name_for_ID))

navy_metrics = na.omit(data_metrics)
navy_metrics = navy_metrics[navy_metrics$Org=='NAVY',]

# Lden per site ----------------------------------------------------------------
# During days of activity/inactivity, what are overall levels throughout the region?
# Dependencies: any dataset for overview, only NAVY dataset for in/activity detail

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

# Active vs inactive view
activity_colors = c('salmon','gray')
p_lden_site_active = ggplot() +
  geom_boxplot(data=combined_data_metrics, mapping=aes(x=reorder(ID, Lden, FUN=energyavg), y=Lden, fill=Activity), color='gray30', outlier.size=0.6) +
  stat_summary(data=combined_data_metrics, mapping=aes(x=reorder(ID, Lden, FUN=energyavg), y=Lden, shape='Energy average', fill=Activity), fun='energyavg', geom='point', size=3, color='gray30', position=position_dodge(width=0.75)) +
  scale_shape_manual('', values=c('Energy average'=21)) +
  scale_fill_manual(name='Flight activity', values=activity_colors) +
  labs(title='Lden per site by flight activity', subtitle='(Weekday vs weekend)', x ='Site', y ='Lden (dBA)') +
  coord_flip()
print(p_lden_site_active)
ggsave(p_lden_site_active, file=paste0(ggsave_output_path, 'lden_site_activity.png'), width=ggsave_width, height=ggsave_height)

# Active vs inactive difference
active_inactive_diff = tapply(active_site_date_metrics$Lden, active_site_date_metrics$ID, energyavg) - tapply(inactive_site_date_metrics$Lden, inactive_site_date_metrics$ID, energyavg)
print(data.frame(
  ID=names(active_inactive_diff),
  diff=active_inactive_diff,
  active = tapply(active_site_date_metrics$Lden, active_site_date_metrics$ID, energyavg),
  inactive = tapply(inactive_site_date_metrics$Lden, inactive_site_date_metrics$ID, energyavg)
))

# Lnight per site --------------------------------------------------------------
# Dependencies: any dataset for overview, only NAVY dataset for in/activity detail

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
  geom_boxplot(data=combined_night_data_metrics, mapping=aes(x=reorder(ID, Lden_Lnight, FUN=energyavg), y=Lden_Lnight), outlier.size=0.9) +
  stat_summary(data=combined_night_data_metrics, mapping=aes(x=reorder(ID, Lden_Lnight, FUN=energyavg), y=Lden_Lnight, shape='Energy average'), fun='energyavg', geom='point', size=3, fill='white') +
  scale_shape_manual('', values=c('Energy average'=21)) +
  labs(title='Lnight per site', x ='Site', y ='Lnight (dBA)', color='Airfield') +
  coord_flip()
print(p_lnight_site_all)
ggsave(p_lnight_site_all, file=paste0(ggsave_output_path, 'lnight_site.png'), width=ggsave_width, height=ggsave_height)

# Active vs inactive view
p_lnight_site_active = ggplot() + 
  geom_boxplot(data=combined_night_data_metrics, mapping=aes(x=reorder(ID, Lden_Lnight, FUN=energyavg), y=Lden_Lnight, fill=Activity), color='gray30', outlier.size=0.6) +
  stat_summary(data=combined_night_data_metrics, mapping=aes(x=reorder(ID, Lden_Lnight, FUN=energyavg), y=Lden_Lnight, shape='Energy average', fill=Activity), fun='energyavg', geom='point', size=3, color='gray30', position=position_dodge(width=0.75)) +
  scale_shape_manual('', values=c('Energy average'=21)) +
  scale_fill_manual(name='Flight activity', values=activity_colors) +
  labs(title='Lnight per site by flight activity', subtitle='(Weekday vs weekend)', x ='Site', y ='Lnight (dBA)') +
  coord_flip()
print(p_lnight_site_active)
ggsave(p_lnight_site_active, file=paste0(ggsave_output_path, 'lnight_site_activity.png'), width=ggsave_width, height=ggsave_height)

# Active vs inactive difference
active_inactive_diff_Lnight = tapply(active_night_site_date_metrics$Lden_Lnight, active_night_site_date_metrics$ID, energyavg) - tapply(inactive_night_site_date_metrics$Lden_Lnight, inactive_night_site_date_metrics$ID, energyavg)
data.frame(
  ID=names(active_inactive_diff_Lnight),
  diff=active_inactive_diff_Lnight
)

