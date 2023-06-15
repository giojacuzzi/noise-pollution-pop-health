## Temporal distribution
# When does the noise occur?

source('global.R')
source('metrics/metrics.R')

data_sites   = get_data_sites()
data_metrics = get_data_metrics()
data_metrics$Field = unlist(lapply(data_metrics$ID, get_field_name_for_ID))
data_events  = get_data_events()
data_ops     = get_data_ops()

## Monitoring site map  --------------------------------------------------------
# Display all sites that have calculated metrics

sites_with_metrics = data_sites[data_sites$ID %in% unique(data_metrics$ID),]
mapviewOptions(legend.pos='bottomright')
mapview(
  sites_with_metrics,
  xcol='Longitude', ycol='Latitude', zcol='Org',
  layer.name = 'Organization', crs=4269, grid=F, legend=T,
  col.regions=c('darkgoldenrod2', 'navy', 'green3', 'darkturquoise')
) %>% addStaticLabels(label=sites_with_metrics$ID, direction='top')

# Average Lden and total/field ops sentinel Whidbey sites all days -------------
# When is the navy active throughout the week, and how does it affect overall levels?
# Dependencies: NAVY dataset

sentinel_sites = c(
  '8B_SG', # Ault Field
  '24A_B'  # OLF Coupeville
)
navy_metrics = na.omit(data_metrics)
navy_metrics = navy_metrics[navy_metrics$Org=='NAVY',]
sentinel_metrics_ault = navy_metrics[navy_metrics$ID==sentinel_sites[1],]
sentinel_metrics_coup = navy_metrics[navy_metrics$ID==sentinel_sites[2],]

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

names = c(paste(get_site_name_for_ID(sentinel_sites[1]), '(Ault)'), paste(get_site_name_for_ID(sentinel_sites[2]), '(Coup)'))

df_mean_ops_lden_day = data.frame(
  Day   = factor(rep(days,2), levels=days),
  Field = factor(c(rep('Ault',7), rep('Coup',7))),
  Name  = factor(c(rep(names[1],7), rep(names[2],7))),
  Ops   = c(mean_ops_day_ault, mean_ops_day_coup),
  Lden  = c(energyavg_lden_day_ault, energyavg_lden_day_coup)
)

p_mean_ops_field_day = ggplot() +
  geom_bar(data=df_mean_ops_lden_day, aes(x=Day, y=Ops, fill=Field), stat='identity', position='dodge', alpha=0.9) +
  labs(title='Average flight operations per day', x='', y='Operations')
p_mean_lden_field_day = ggplot() +
  geom_line(data=df_mean_ops_lden_day, aes(x=Day, y=Lden, group=Name, color=Name), stat='identity') +
  scale_y_continuous(name='Lden (dBA)', limits=c(50,90), oob=rescale_none) +
  labs(title='Average daily Lden', subtitle='(Sentinel sites)')
print(p_mean_ops_field_day / p_mean_lden_field_day)
ggsave(p_mean_ops_field_day / p_mean_lden_field_day, file=paste0(ggsave_output_path, 'lden_ops_daily.png'), width=ggsave_width, height=ggsave_height)

# Average operations per week --------------------------------------------------
# For the navy monitoring periods
print('Average operations per week')
print(paste('Ault Field:', sum(mean_ops_day_ault)))
print(paste('OLF Coupeville:', sum(mean_ops_day_coup)))
print(paste('Total:', sum(mean_ops_day_ault) + sum(mean_ops_day_coup)))

# Average active day Leq and ops per hour --------------------------------------
# Dependencies: NAVY dataset
# TODO: show distribution as well, some days are much worse and late at night

ops_hour_ault = c()
for (hour in hours) ops_hour_ault = c(ops_hour_ault, sum(data_ops[data_ops$Field=='Ault',]$Hour==hour))
mean_ops_hour_ault = ops_hour_ault / length(unique(data_ops[data_ops$Field=='Ault',]$Date))
ops_hour_coup = c()
for (hour in hours) ops_hour_coup = c(ops_hour_coup, sum(data_ops[data_ops$Field=='Coup',]$Hour==hour))
mean_ops_hour_coup = ops_hour_coup / length(unique(data_ops[data_ops$Field=='Coup',]$Date))

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

active_ault_hour_metrics = subset(active_site_date_metrics[active_site_date_metrics$ID==sentinel_sites[1],], select=Leq00:Leq23)
energyavg_leq_hour_ault = sapply(active_ault_hour_metrics, energyavg)
active_coup_hour_metrics = subset(active_site_date_metrics[active_site_date_metrics$ID==sentinel_sites[2],], select=Leq00:Leq23)
energyavg_leq_hour_coup = sapply(active_coup_hour_metrics, energyavg)

df_mean_ops_hour = data.frame(
  Hour  = factor(rep(hours,2), levels=hours),
  Field = factor(c(rep('Ault',24), rep('Coup',24))),
  Name  = factor(c(rep(names[1],24), rep(names[2],24))),
  Ops   = c(mean_ops_hour_ault, mean_ops_hour_coup),
  Leq   = c(energyavg_leq_hour_ault, energyavg_leq_hour_coup)
)

p_mean_ops_field_hour = ggplot() +
  geom_bar(data=df_mean_ops_hour, aes(x=Hour, y=Ops, fill=Field), stat='identity', position='dodge', alpha=0.9) +
  labs(title='Average flight operations per hour', subtitle='(Active day)', x='', y='Operations')
p_mean_leq_field_hour = ggplot() +
  geom_line(data=df_mean_ops_hour, aes(x=Hour, y=Leq, group=Name, color=Name), stat='identity') +
  scale_y_continuous(name='Leq (dBA)', limits=c(40,90), oob=rescale_none) +
  geom_rect(aes(xmin='00', xmax = '07', ymin = -Inf, ymax = Inf), fill = 'blue', alpha = 0.09) +
  geom_rect(aes(xmin='19', xmax = '22', ymin = -Inf, ymax = Inf), fill = 'purple', alpha = 0.06) +
  geom_rect(aes(xmin='22', xmax = '23', ymin = -Inf, ymax = Inf), fill = 'blue', alpha = 0.09) +
  labs(title='Average hourly Leq', subtitle='(Sentinel sites)')
print(p_mean_ops_field_hour / p_mean_leq_field_hour)
ggsave(p_mean_ops_field_hour / p_mean_leq_field_hour, file=paste0(ggsave_output_path, 'lden_ops_hourly.png'), width=ggsave_width, height=ggsave_height)

# Average operations per hour  -------------------------------------------------
# For the navy monitoring periods
print('Average operations per hour')
print(paste('Ault Field:', round(sum(mean_ops_hour_ault))))
print(paste('OLF Coupeville:', round(sum(mean_ops_hour_coup))))
print(paste('Total:', round(sum(mean_ops_hour_ault) + sum(mean_ops_hour_coup))))

# Yearly FCLP operations reports -----------------------------------------------
# Copied from "Consolidated Responsive Records" pdf
# Acoustic night is 22:00 onward
ault_fclps_2021 = data.frame(
  Day=c(
    730, 0, 0, 352, 670, 208, 0, 488, 0, 30, 166, 114
  ),
  Evening=c(
    402, 0, 0, 552, 226, 176, 0, 684, 0, 74, 662, 60
  ),
  Night=c(
    0,   0, 0, 170, 218, 50,  0, 518, 0, 0,  0,   0
  ),
  Field=rep('Ault', 12),
  Month=months,
  Year=rep(2021, 12)
)
sum(ault_fclps_2021$Day) + sum(ault_fclps_2021$Evening) + sum(ault_fclps_2021$Night)

coup_fclps_2021 = data.frame(
  Day=c(
    376, 0, 600, 630, 296, 1000, 360, 888, 252, 310, 626, 0
  ),
  Evening=c(
    936, 0, 248, 586, 108, 674,  0,   586, 238, 530, 570, 0 
  ),
  Night=c(
    0,   0, 0,   0,   182, 750,  0,   258, 0,   0,   0,   0
  ),
  Field=rep('Coup', 12),
  Month=months,
  Year=rep(2021, 12)
)
sum(coup_fclps_2021$Day) + sum(coup_fclps_2021$Evening) + sum(coup_fclps_2021$Night)

ault_fclps_2022 = data.frame(
  Day=c(
    166, 534, 450, 660, 258, 0,   622, 140, 0, 0, 0, 0
  ),
  Evening=c(
    42,  536, 338, 108, 136, 128, 38,  0,   0, 0, 0, 0
  ),
  Night=c(
    0,   0,   0,   34,  84,  222, 110, 0,   0, 0, 0, 0
  ),
  Field=rep('Ault', 12),
  Month=months,
  Year=rep(2022, 12)
)
sum(ault_fclps_2022$Day) + sum(ault_fclps_2022$Evening) + sum(ault_fclps_2022$Night)

coup_fclps_2022 = data.frame(
  Day=c(
    226, 714,  692, 120, 334, 340, 100, 0, 0, 0, 0, 0
  ),
  Evening=c(
    206, 1312, 706, 748, 434, 426, 0,   0, 0, 0, 0, 0
  ),
  Night=c(
    0,   0,    154, 412, 188, 460, 0,   0, 0, 0, 0, 0
  ),
  Field=rep('Coup', 12),
  Month=months,
  Year=rep(2022, 12)
)
sum(coup_fclps_2022$Day) + sum(coup_fclps_2022$Evening) + sum(coup_fclps_2022$Night)

fclp_ops = rbind(ault_fclps_2021, coup_fclps_2021, ault_fclps_2022, coup_fclps_2022)
fclp_ops$Field = factor(fclp_ops$Field)
fclp_ops$Month = factor(fclp_ops$Month)
fclp_ops$Year = factor(fclp_ops$Year)
print(fclp_ops)

ggplot(gather(fclp_ops[fclp_ops$Year==2021,], Period, Operations, Day:Night, factor_key=T), aes(fill=Period, y=Operations, x=Month)) +
  geom_bar(position='stack', stat='identity') +
  labs(title='Total FCLP operations, both fields, 2021')

ggplot(gather(fclp_ops[fclp_ops$Year==2022,], Period, Operations, Day:Night, factor_key=T), aes(fill=Period, y=Operations, x=Month)) +
  geom_bar(position='stack', stat='identity') +
  labs(title='Total FCLP operations, both fields, 2022')

# Hours of the day that included a noise event >95 dB LAeq (across all noise events)
threshold = 95
loud_events = data_events[data_events$LAeq>threshold, ]
loud_events$Hour = factor(loud_events$Hour)
loud_events_hour = data.frame(
  Hour=names(summary(loud_events$Hour)),
  Events=summary(loud_events$Hour)
)
leh = c()
for (hour in hours) leh = c(leh, max(loud_events_hour[loud_events_hour$Hour==hour, 'Events'],0))
loud_events_hour = data.frame(
  Hour=hours,
  Events=leh
)
ggplot(loud_events_hour, aes(x=Hour,y=Events)) + geom_bar(stat='identity') + labs(title=paste0('Number of ', threshold, '+ dB noise events per hour'))
