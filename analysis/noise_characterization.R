## Noise characterization
# How is the noise quantitatively characterized?

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
threshold=0
ggplot(data_events[data_events$LAeq_Lmax>=threshold & data_events$Org!='SDA',], aes(x=ID, y=LAeq_Lmax)) +
  geom_boxplot(fill="slateblue", alpha=0.0) +
  geom_jitter(color="black", size=0.4, alpha=0.5) +
  coord_flip()

## Max events per site ---------------------------------------------------------
# TODO
events_lmax = data_events[!is.na(data_events$LAeq_Lmax),]
events_lmax = events_lmax[events_lmax$Org!='SDA',] # do not include SDA data in lmax calculations due to instrumentation error
library(dplyr)
events_lmax = events_lmax %>% group_by(ID) %>% slice(which.max(LAeq_Lmax))
# max event recorded was 561 KysH (119.8 dBA max, 136.2 dBC peak)
source('data/events/evaluate_events.R')
for (i in 1:nrow(events_lmax)) {
  id   = events_lmax[i,'ID'][[1]]
  date = substr(as.character(events_lmax[1,'TimeStart'][[1]]), 1, 10)
  num  = events_lmax[i,'X'][[1]]
  plot_events(id, date, num)
}
# Events manually visually verified as aircraft with `plot_events(id, date, num)`:
# TODO: Those with '???' need to be verified with spectral data
# 753 20B_SG
# 9758 24A_B
# 10304 25B_T
# 13327 26B_SG
# 16290 27A_SG
# 18086 2B_T   2020-12-15
# 20831 33_SG  2020-12-16 <<<<<< ???
# 24947 3A_T   2021-04-01
# 26233 5B_SG  2020-12-14 <<<<<< ???
# 134 AdmB   2019-06-24
# 302 CrcF   2019-07-03
# 764 EBLA0… 2015-06-29
# 2508 EBLA0… 1992-07-13 <<<<<<<< ???
# 561 KysH   2019-06-18
# 914 LckS   2016-02-02
# 999 LnPM   2019-06-18
# 1079 RshF   2019-07-01
# 1148 TrnC   2019-06-18

## 
# Subset of a 4-day period (2019-06-18 through 21) that included 10 sorties
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

# buffer = 30
# event = data_events[e,]
# event_data = data[which(data$Time==(event$TimeStart-buffer)):which(data$Time==(event$TimeEnd+buffer)),]
# 
# # Leq time series
# p_leq = ggplot(event_data) +
#   geom_line(aes(x=Time, y=LAeq)) +
#   labs(x='Time', y='Leq (dBA)')
# 
# # Spectral heatmap
# spectrum = event_data[,c(1, 26:61)] # NOTE: 6.3 Hz starts at index 26, 20 Hz at 31
# names(spectrum) = gsub('1/3 LZeq ', '', names(spectrum))
# 
# spectrum_total = data.frame()
# for (s in 1:nrow(spectrum)) {
#   sec = as.POSIXct(spectrum$Time[s])
#   band = rownames(t(spectrum[s,c(-1)]))
#   lzeq = unname(spectrum[s,c(-1)])
#   spectrum_sec = data.frame(
#     sec,
#     band,
#     t(lzeq)
#   )
#   rownames(spectrum_sec) = c()
#   colnames(spectrum_sec) = c('Time', 'Band', 'LZeq')
#   spectrum_total = rbind(spectrum_total, spectrum_sec)
# }
# spectrum_total$Band = as.character(as.numeric(spectrum_total$Band))
# spectrum_total$Band = factor(spectrum_total$Band)
# sorted_levels = as.character(sort(as.numeric(levels(spectrum_total$Band))))
# spectrum_total$Band = factor(spectrum_total$Band, levels=sorted_levels)
# 
# freq_labels = c( # levels(spectrum_total$Band)
#   "",   "8",     "",   "12.5",  "",    "20",    "",    "31.5",  "",    "50",    "",    "80",    "",   "125",   "",   "200",   "",   "315",   "",   "500",   "",   "800",   "",  "1250",  "",  "2000",  "",  "3150",  "",  "5000",  "",  "8000",  "", "12500", "", "20000"
# )
# 
# p_spectral = ggplot(spectrum_total, aes(x=Time, y=Band, fill=LZeq)) +
#   geom_tile() +
#   scale_fill_viridis(option='A') +
#   scale_y_discrete(labels=freq_labels) +
#   labs(title='', x='', y='Frequency (Hz)', fill='Leq (dBZ)')
# 
# print(p_spectral / p_leq)
