source('global.R')
source('data/metrics/metrics.R')
library(patchwork)

source('data/load/load_site_date.R')
id = '24A_B'
date = '2021-08-10'
data = load_site_date(id, date)
data$Time = as.POSIXct(data$Time)

time_start = '2021-08-10 20:15:00'
time_end = '2021-08-10 21:30:00'

  data_selection = data[which(data$Time==as.POSIXct(time_start, tz='UTC')):which(data$Time==as.POSIXct(time_end, tz='UTC')),]
    
    # Leq time series
    p_leq = ggplot(data_selection) +
      geom_line(aes(x=Time, y=LAeq)) +
      labs(x='Time', y='Leq (dBA)')
    
    # Spectral heatmap
    spectrum = data_selection[,c(1, 26:61)] # NOTE: 6.3 Hz starts at index 26, 20 Hz at 31
    names(spectrum) = gsub('1/3 LZeq ', '', names(spectrum))
    
    spectrum_total = data.frame()
    for (s in 1:nrow(spectrum)) {
      sec = as.POSIXct(spectrum$Time[s])
      band = rownames(t(spectrum[s,c(-1)]))
      lzeq = unname(spectrum[s,c(-1)])
      spectrum_sec = data.frame(
        sec,
        band,
        t(lzeq)
      )
      rownames(spectrum_sec) = c()
      colnames(spectrum_sec) = c('Time', 'Band', 'LZeq')
      spectrum_total = rbind(spectrum_total, spectrum_sec)
    }
    spectrum_total$Band = as.character(as.numeric(spectrum_total$Band))
    spectrum_total$Band = factor(spectrum_total$Band)
    sorted_levels = as.character(sort(as.numeric(levels(spectrum_total$Band))))
    spectrum_total$Band = factor(spectrum_total$Band, levels=sorted_levels)
    
    freq_labels = c( # levels(spectrum_total$Band)
      "",   "8",     "",   "12.5",  "",    "20",    "",    "31.5",  "",    "50",    "",    "80",    "",   "125",   "",   "200",   "",   "315",   "",   "500",   "",   "800",   "",  "1250",  "",  "2000",  "",  "3150",  "",  "5000",  "",  "8000",  "", "12500", "", "20000"
    )
    
    p_spectral = ggplot(spectrum_total, aes(x=Time, y=Band, fill=LZeq)) +
      geom_tile() +
      scale_fill_viridis(option='A') +
      scale_y_discrete(labels=freq_labels) +
      labs(title='', x='', y='Frequency (Hz)', fill='Leq (dBZ)')
    
    print(p_spectral / p_leq)
    ggsave(p_spectral / p_leq, file=paste0('analysis/_output/', 'session_example.png'), width=10, height=5)
