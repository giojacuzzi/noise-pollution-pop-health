source('global.R')
source('metrics/metrics.R')
library(patchwork)

source('figures/fig_global.R')
source('data/load/load_site_date.R')
id = '24A_B'
date = '2021-08-10'
data = load_site_date(id, date)
data$Time = as.POSIXct(data$Time)

time_start = '2021-08-10 20:35:00'
time_end = '2021-08-10 21:25:00'
time_start_event = '2021-08-10 21:17:53'
time_end_event = '2021-08-10 21:20:40'

data_session = data[which(data$Time==as.POSIXct(time_start, tz='UTC')):which(data$Time==as.POSIXct(time_end, tz='UTC')),]
data_event = data_session[data_session$Time >= as.POSIXct(time_start_event, tz='UTC') & data_session$Time <= as.POSIXct(time_end_event, tz='UTC'),]

color_event_marker = alpha('white', 0.5)

# Leq time series
plot_Leq = function(l) {
  p_leq = ggplot(l) +
    geom_line(aes(x=Time, y=LAeq), color='#222222') +
    labs(x='Time', y=expression(L[eq]~'dB('*A*')')) +
    scale_x_datetime(expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(30,120,10), limits = c(25, 120), expand = c(0, 0)) +
    # geom_vline(xintercept = as.POSIXct(time_start_event, tz='UTC'), linetype = 'dashed', color='gray') +
    # geom_vline(xintercept = as.POSIXct(time_end_event, tz='UTC'), linetype = 'dashed', color='gray') +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size=5),
      axis.text.y = element_text(size=5),
      text=element_text(size=text_size_max)
    ); return(p_leq)
}

p_session_Leq = plot_Leq(data_session); p_session_Leq
# p_event_Leq = plot_Leq(data_event); p_event_Leq

# Spectral heatmap
plot_spectrum = function(l) {
  
  spectrum = l[,c(1, 26:61)] # NOTE: 6.3 Hz starts at index 26, 20 Hz at 31
  names(spectrum) = gsub('1/3 LZeq ', '', names(spectrum))
  
  s = data.frame()
  for (i in 1:nrow(spectrum)) {
    sec = as.POSIXct(spectrum$Time[i])
    band = rownames(t(spectrum[i,c(-1)]))
    lzeq = unname(spectrum[i,c(-1)])
    spectrum_sec = data.frame(
      sec,
      band,
      t(lzeq)
    )
    rownames(spectrum_sec) = c()
    colnames(spectrum_sec) = c('Time', 'Band', 'LZeq')
    s = rbind(s, spectrum_sec)
  }
  s$Band = as.character(as.numeric(gsub('X1.3.LZeq.', '', s$Band)))
  s$Band = factor(s$Band)
  sorted_levels = as.character(sort(as.numeric(levels(s$Band))))
  s$Band = factor(s$Band, levels=sorted_levels)

  freq_labels = c( # levels(spectrum_total$Band)
    "",   "8",     "",   "12.5",  "",    "20",    "",    "31.5",  "",    "50",    "",    "80",    "",   "125",   "",   "200",   "",   "315",   "",   "500",   "",   "800",   "",  "1250",  "",  "2000",  "",  "3150",  "",  "5000",  "",  "8000",  "", "12500", "", "20000"
  )
  
  p_spectral = ggplot(s, aes(x=factor(Time), y=Band, fill=LZeq)) +
    geom_raster() +
    scale_fill_viridis(option='A') +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(labels=freq_labels, expand = c(0, 0)) +
    # geom_vline(xintercept = factor(as.POSIXct(time_start_event, tz='UTC')), linetype = 'dashed', color=color_event_marker) +
    # geom_vline(xintercept = factor(as.POSIXct(time_end_event, tz='UTC')), linetype = 'dashed', color=color_event_marker) +
    labs(title='', x='Time', y='Frequency (Hz)', fill=expression(L[eq]~'dB('*Z*')')) +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size=5),
      text=element_text(size=text_size_max),
      legend.position='none'
    )
  return(p_spectral)
}

data_events = get_data_events()
data_events = data_events[
  order(data_events[,'LAeq_Lmax'], data_events[,'Duration'], decreasing = T),
]
data_events = data_events[data_events$Org == 'NAVY', ]

i = 2
event = data_events[i,]
id = event$ID
date = event$Date
time_start = event$TimeStart
time_end = event$TimeEnd
data = load_site_date(id, date)
data = data[data$Time >= time_start & data$Time <= time_end, ]

p_event_Leq = plot_Leq(data)
p_event_spectrum = plot_spectrum(data)
p_event_combined = p_event_Leq / p_event_spectrum + plot_annotation(tag_levels = 'A'); p_event_combined
ggsave(p_event_combined, file=paste0('figures/_output/', 'noise_event.png'), width=8, height=5)

ggsave(filename = glue('{output_path}/noise_event.eps'), 
       device = 'eps', units = 'cm', dpi = 300, 
       width = fig_size_single, height = fig_size_single + 0.0,
       plot = p_event_combined + theme())

p_session_spectrum = plot_spectrum(data_session); p_session_spectrum
p_session_combined = (p_session_Leq / p_session_spectrum) + plot_annotation(tag_levels = 'A'); p_session_combined
ggsave(p_session_combined, file=paste0('figures/_output/', 'noise_session.png'), width=8, height=5)

ggsave(filename = glue('{output_path}/noise_session.eps'), 
       device = 'eps', units = 'cm', dpi = 300, 
       width = fig_size_single, height = fig_size_single + 0.0,
       plot = p_session_combined + theme())

