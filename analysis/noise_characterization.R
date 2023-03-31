## Noise characterization
# How is the noise quantitatively characterized?
# Requirements: PHI database

source('global.R')
source('data/metrics/metrics.R')
library(mapview)
library(leafem)

data_sites  = get_data_sites()
data_events = get_data_events()
data_metrics = get_data_metrics()

## Monitoring site map  --------------------------------------------------------
# Display all sites that have calculated events

sites_with_events = data_sites[data_sites$ID %in% unique(data_events$ID),]
mapviewOptions(legend.pos='bottomright')
mapview(
  sites_with_events,
  xcol='Longitude', ycol='Latitude', zcol='Org',
  layer.name = 'Organization', crs=4269, grid=F, legend=T,
  col.regions=c('darkgoldenrod2', 'navy', 'green3', 'darkturquoise')
) %>% addStaticLabels(label=sites_with_events$ID, direction='top')

## Box plot event LAeq_Lmax per site > a threshold value
threshold=80
ggplot(data_events[data_events$LAeq_Lmax>=threshold & data_events$Org!='SDA',], aes(x=ID, y=LAeq_Lmax)) +
  geom_boxplot(fill="slateblue", alpha=0.0) +
  geom_jitter(color="black", size=0.4, alpha=0.5) +
  coord_flip()

## Max events per site ---------------------------------------------------------
# TODO
events_lmax = data_events[!is.na(data_events$LAeq_Lmax),]
# discard SDA data in lmax calculations due to instrumentation error
events_lmax = events_lmax[events_lmax$Org!='SDA',]
# discard dates before 2015, when primary Growler activity began
events_lmax = events_lmax[events_lmax$TimeStart >= as.POSIXct('2015-01-01 00:00:00', tz='UTC'), ]
# discard events from site 33_SG (no discernable aircraft events recorded)
events_lmax = events_lmax[events_lmax$ID!='33_SG',]

library(dplyr)
source('data/events/evaluate_events.R')
events_lmax = events_lmax %>% group_by(ID) %>% slice(which.max(LAeq_Lmax))

# Events manually visually verified as aircraft with `plot_events(id, date, num)`
# max event recorded was 561, JGL, KysH (119.8 dBA max, 136.2 dBC peak) at 23:56:55
plot_events(
  events_lmax[14, 'ID'][[1]],
  events_lmax[14, 'Date'][[1]],
  events_lmax[14, 'X'][[1]]
)
# max navy event recorded was 9758, 24A_B, 2021-08-10  (115.1)
plot_events(
  events_lmax[2, 'ID'][[1]],
  events_lmax[2, 'Date'][[1]],
  events_lmax[2, 'X'][[1]]
)

## 
# Subset of a 4-day period (2019-06-18 through 21) that included 10 FCLP sessions
source('data/load/load_site_date.R')
data_date_1 = load_site_date('KysH', '2019-06-18')
data_date_2 = load_site_date('KysH', '2019-06-19')
data_date = rbind(data_date_1, data_date_2)
# data_date = na.omit(data_date)
start_idx = which(data_date$Time==as.POSIXct('2019-06-18 20:00:00', tz='UTC'))
end_idx   = which(data_date$Time==as.POSIXct('2019-06-19 00:25:00', tz='UTC'))
data_date = data_date[start_idx:end_idx,]
ggplot(data_date, aes(x=Time, y=LAeq)) +
  geom_line()

## Frequency spectrum of FCLP event --------------------------------------------
# Take from example date, 2020-12-15, chosen for its prevalence of FCLP aircraft events across multiple sites near Coupeville airbase
# Energy average spectrum per event for the day, then energy average across all of those events to have a representative “typical FCLP” at that location. Include average event peak values and the max event peak value as well.
ids = c('26B_SG','25B_T','20B_SG','27A_SG', '24A_B')
date = '2020-12-15'

# FCLP session times for 2020-12-15
# 12:37-13:11
# 14:29-15:01
# 17:01-18:10
# 18:31-19:40

for (id in ids) {
  events_sesh=data_events[
    data_events$ID==id &
      data_events$Date=='2020-12-15' &
      (# Session 1
        (data_events$TimeStart>as.POSIXct('2020-12-15 12:36:00', tz='UTC') &
           data_events$TimeStart<as.POSIXct('2020-12-15 13:12:00', tz='UTC')) |
          # Session 2
          (data_events$TimeStart>as.POSIXct('2020-12-15 14:28:00', tz='UTC') &
             data_events$TimeStart<as.POSIXct('2020-12-15 15:02:00', tz='UTC')) |
          # Session 3
          (data_events$TimeStart>as.POSIXct('2020-12-15 17:00:00', tz='UTC') &
             data_events$TimeStart<as.POSIXct('2020-12-15 18:11:00', tz='UTC')) |
          # Session 4
          (data_events$TimeStart>as.POSIXct('2020-12-15 18:30:00', tz='UTC') &
             data_events$TimeStart<as.POSIXct('2020-12-15 19:41:00', tz='UTC')
          ))
    , ]
  # plot_events('25B_T', date, events_sesh$X)
  
  site_data = load_site_date(id, date)
  session_freq_avg = data.frame()
  for (e in 1:nrow(events_sesh)) {
    event = events_sesh[e,]
    event_data = site_data[which(site_data$Time==event$TimeStart):which(site_data$Time==event$TimeEnd),]
    names(event_data) = gsub('X1.3.LZeq.', '', names(event_data))
    event_data = event_data[,c(1, 26:61)]
    
    event_avg = data.frame(event_data[,-c(1)])
    event_avg = sapply(event_avg, energyavg)
    event_avg = data.frame(
      Band=gsub('X','',names(event_avg)),
      dBZ=event_avg
    )
    session_freq_avg = rbind(session_freq_avg, event_avg)
  }
  
  session_LAeq_Lmax_avg = 0
  session_LCpeak_avg = 0
  session_SEL_avg = 0
  
  session_freq_avg$Band = as.character(as.numeric(session_freq_avg$Band))
  session_freq_avg$Band = factor(session_freq_avg$Band)
  sorted_levels = as.character(sort(as.numeric(levels(session_freq_avg$Band))))
  session_freq_avg$Band = factor(session_freq_avg$Band, levels=sorted_levels)
  session_freq_avg = session_freq_avg%>%group_by(Band)%>%summarise(EnergyAvg=energyavg(dBZ))
  p_sesh = ggplot(as.data.frame(session_freq_avg), aes(x=Band, y=EnergyAvg)) + 
    geom_bar(stat = "identity") +
    labs(title = paste('FCLP Event 1/3 octave band spectrum', id),
         subtitle = paste('LAeq_Lmax', round(energyavg(events_sesh$LAeq_Lmax),1), 'LCpeak', round(energyavg(events_sesh$LCpeak),1), 'SEL', round(energyavg(events_sesh$SEL),1)))
  print(p_sesh)
}
