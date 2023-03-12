source('global.R')

# NOTE: According to Stusnick,
# 0 dB adjustment for onset_rate <= 15 db/sec
# 11*log10(onset_rate) - 12.9 adjustment for 15 db/sec <= onset_rate <= 150 db/sec
# 11 dB adjustment for onset_rate >= 150 db/sec

# https://drive.google.com/drive/u/0/folders/1wWnqevA6S3ANlzIVyh5lu_bcuR0cOybx
# A sound eventis characterized bythe sound exposure level LEA, and the maximum sound pressure level LpASmax or LpAeq1smax

# An automatic sound-monitoring system shall reliably and precisely detect and classify aircraft sound events. The chosen technique shall satisfy the following criteria:
# a) The expanded uncertainty (see Clause 6) of the measured cumulated exposure level of all aircraft sound events shall not exceed 3 dB
# b) At least 50% of true aircraft sound events shall be correctly classified as aircraft sound events
# c) The number of non-aircraft sound events which are incorrectly classified as shall be less than 50% of the true number of aircraft sound events

# For each sound event:
# A-weighted sound exposure level LEA
# Maximum SPL LpASmax and/or LpAeq1smax
# Actual SPL of the event detection threshold Lthreshold if releveant
# Time-history sequence of SPLs

# Data processing stages:
# Continuous measurement >
# Event extraction and classification >
# Event identification (informed by ops info) >
#  > Reports of aircraft sound events, missing, or unidentified

library(dplyr)
find_extrema = function (x, last=F) {
  x_rle <- rle(x) # handle duplicates
  # force first value to be recognized as an extreme
  first_value <- x_rle$values[1] - x_rle$values[2]
  
  # differentiate the series, keep only the sign, and use 'rle' to
  # locate increase or decrease concerning multiple successive values.
  # The result values is a series of (only) -1 and 1.
  # NOTE: with this method, last value will be considered as an extrema
  diff_sign_rle <- c(first_value, diff(x_rle$values)) %>% sign() %>% rle()
  
  # vector for getting the initial positions
  diff_idx <- cumsum(diff_sign_rle$lengths)
  
  # find min/max
  diff_min <- diff_idx[diff_sign_rle$values < 0]
  diff_max <- diff_idx[diff_sign_rle$values > 0]
  
  # get min and max indices from original series
  x_idx <- cumsum(x_rle$lengths)
  if (last) {
    min <- x_idx[diff_min]
    max <- x_idx[diff_max]
  } else {
    min <- x_idx[diff_min] - x_rle$lengths[diff_min] + 1
    max <- x_idx[diff_max] - x_rle$lengths[diff_max] + 1
  }
  # get number of occurences
  min_nb <- x_rle$lengths[diff_min]
  max_nb <- x_rle$lengths[diff_max]
  
  # format result as tibble
  bind_rows(
    tibble(idx = min, Values = x[min], NB = min_nb, Status = 'min'),
    tibble(idx = max, Values = x[max], NB = max_nb, Status = 'max')) %>%
    arrange(.data$idx) %>%
    mutate(Last = last) %>%
    mutate_at(vars(.data$idx, .data$NB), as.integer)
}

# -----------------------------------------------------------------------------

# Sound event detection (5.3.2) criteria:
# a) The sound is not steady state, but also not imulsive (i.e. its duration lies within specified limits)
# b) The sound level exceeds a threshold level by at least a specified amount
# c) When an event terminates, the sound level does not rise again above a specified level within a specified time


# Navy used a detection threshold of L90 + 10 for the hour
# ISO 3.13.4 states that background sound may be estimated by the 95% exceedance level of total sound (LpAS95) 
source('data/metrics/metrics.R')
library(patchwork)

# a) Use moving average (exponential?)
# b) Use L95 exceedance threshold
# c) Multiple peaks splitting

data_events = get_data_events()
data_ops = get_data_ops()

source('data/load/load_site_date.R')
id = 'KysH' #'24A_B'
date = '2019-06-18' #'2021-08-10'
data = load_site_date(id, date)

data$Time = as.POSIXct(data$Time)
data$Hour = format(data$Time, format = '%H')

navy_events = data_events[data_events$SiteID==id & data_events$Date==date,]
navy_ops = data_ops[data_ops$Date==date,]

library(zoo)

events = data.frame()

# NOTE: Navy threshold is L90 + 10 of each hour +/- 30 min 
# threshold = LxFromLevels(data$LAeq, 90) + 10

# NOTE: ISO standard 20906:2009 suggests background residual sound may be estimated by L95, and aircraft maxima should be at least 15 dB above the residual sound
# As some data are not entire-day recordings and thus lack an actual reference background, take the minimum of this and X, a baseline 35 dB + 15 ambient value
threshold_buffer = 15
threshold_min_ambience = 35 + threshold_buffer
threshold = min(LxFromLevels(na.omit(data$LAeq), 95) + threshold_buffer, threshold_min_ambience)[1]

# Replace any missing LAeq measurements with 0.0
data[is.na(data$LAeq),'LAeq'] = 0.0

# 10-second moving average
data$Lma = rollmean(data$LAeq, 10, align='center', fill=threshold-threshold_buffer)

sec = 1
while (sec<=nrow(data)) {
  if (data$Lma[sec] > threshold) {
    message(paste('Event start found:', data$Time[sec]))
    event_start = sec
    idx_start = sec
    under_count = 0
    while (sec<=nrow(data) & under_count < 5) { # < threshold for 5 sec
      while (is.na(data$Lma[sec])) sec = sec + 1
      while (sec<=nrow(data) & data$Lma[sec] > threshold) {
        under_count = 0
        sec = sec + 1
      }
      # below threshold again
      under_count = under_count + 1
      sec = sec + 1
    }
    message(paste('Event end found:', data$Time[sec]))
    event_end = min(nrow(data), sec)
    idx_end = min(nrow(data), sec)
    idx_peaks = find_extrema(data$Lma[idx_start:idx_end])
    idx_peaks = unname(unlist(idx_peaks[idx_peaks$Status=='max','idx']))
    idx_peaks = idx_peaks + idx_start - 1
    
    # Find all local maxima that are at least `peak_prominence` dB greater than their lowest neighboring minima
    peak_prominence = 10
    if (length(idx_peaks) > 1) {
      trimmed_idx_peaks = c()
      for (i in idx_peaks) { # for each local maxima
        local_max_level = data[i,'Lma']
        if (nrow(events) + 1 == 26) {
          message(local_max_level)
        }
        l = i - 1
        while (l >= idx_start
               & data[l,'Lma'] <= local_max_level
               & data[l,'Lma'] > (local_max_level-peak_prominence)) {
          l = l - 1
        }
        r = i + 1
        while (r <= idx_end
               & data[r,'Lma'] <= local_max_level
               & data[r,'Lma'] > (local_max_level-peak_prominence)) {
          r = r + 1
        }
        if ((local_max_level - data[l,'Lma'] >= peak_prominence)
            & (local_max_level - data[r,'Lma'] >= peak_prominence)) {
          trimmed_idx_peaks = append(trimmed_idx_peaks, i)
        }
      }
      if (length(trimmed_idx_peaks) == 0) {
        idx_peaks = idx_peaks[which(data[idx_peaks,'Lma']==max(data[idx_peaks,'Lma']))][1]
      } else {
        idx_peaks = trimmed_idx_peaks
      }
    }
    
    # If no 'prominent' maxima, just take the max peak
    if (length(idx_peaks)==0) {
      idx_peaks = idx_peaks[which(data$Lma[idx_peaks]==max(data$Lma[idx_peaks]))[1]]
    }
    
    # Keep only peaks within 10 sec of each other
    trimmed_idx_peaks = c()
    trimmed_idx_peaks = append(trimmed_idx_peaks, idx_peaks[1])
    for (i in 1:length(idx_peaks)) {
      curr_peak_idx = idx_peaks[i]
      next_peak_idx = ifelse(i < length(idx_peaks), idx_peaks[i+1], idx_end)
      if (next_peak_idx - curr_peak_idx > 10 & next_peak_idx != idx_end) {
        trimmed_idx_peaks = append(trimmed_idx_peaks, next_peak_idx)
      }
    }
    idx_peaks = trimmed_idx_peaks
    
    # Between each orange peak (and start/1st, last/end), find the minimum Lmc. That is the split point.
    valleys = c()
    for (i in 1:length(idx_peaks)) {
      curr_peak_idx = idx_peaks[i]
      next_peak_idx = ifelse(i < length(idx_peaks), idx_peaks[i+1], idx_end)
      valleys = append(valleys, which(data$Lma[curr_peak_idx:next_peak_idx]==min(data$Lma[curr_peak_idx:next_peak_idx]))[1] + curr_peak_idx - 1)
    }
    
    # Plot the entire event with a +/- 45 sec buffer
    buff_start = max(1, idx_start-45)
    buff_end = min(nrow(data), idx_end+45)
    p_time = ggplot(data[buff_start:buff_end,]) +
      labs(title=paste('Peak threshold event', nrow(events) + 1)) +
      geom_line(aes(x=Time, y=LAeq)) +
      geom_line(aes(x=Time, y=Lma), color='magenta') +
      geom_vline(xintercept=data[idx_start,'Time'], color='blue') +
      geom_vline(xintercept=data[idx_end,'Time'], color='blue') +
      geom_hline(yintercept=threshold, color='gray') +
      geom_vline(xintercept=data[idx_peaks, 'Time'], color='orange', linetype='dotted') +
      geom_vline(xintercept=data[valleys, 'Time'], color='purple', linetype='dotted')
    plot(p_time)
    
    # readline('Peak threshold event plotted. Press [enter] to continue...')
    
    # -------------------------- Multi-event
    if (length(idx_peaks)>1) {
      message(paste('  Splitting event into', length(idx_peaks),'...'))
    }
    
    for (i in 1:length(valleys)) {
      idx_end = ifelse(i<length(valleys), valleys[i], event_end)
      
      time_start = data$Time[idx_start]
      time_end = data$Time[idx_end]
      levels = data$LAeq[idx_start:idx_end]
      LAeq_Lmax = max(levels)
      idx_lmax = which(data[idx_start:idx_end,'LAeq']==LAeq_Lmax)[1]
      lstart = data$LAeq[idx_start]
      onset = (LAeq_Lmax - lstart)/(idx_lmax) # dBA per sec
      if (lstart == 0.0) {
        # There was missing data, onset is not able to be calculated
        onset = NA
      }
      onset = round(onset, 1)
      event = data.frame(
        TimeStart=time_start,
        TimeEnd=time_end,
        Duration=as.numeric(difftime(time_end, time_start, units='secs')),
        LAeq=round(LeqTotal(levels),1),
        SEL=round(SelFromLevels(levels),1),
        LAeq_Lmax=LAeq_Lmax,
        LAFmax=max(data$LAFmax[idx_start:idx_end]),
        LCpeak=max(data$LCpeak[idx_start:idx_end]),
        Onset=onset
      )
      events = rbind(events, event)
      # message(paste('nrow event/events ', nrow(event), '/', nrow(events)))
      
      # Plot sub-events with a +/- 45 sec buffer
      buff_start = max(1, idx_start-45)
      buff_end = min(nrow(data), idx_end+45)
      p_time = ggplot(data[buff_start:buff_end,]) +
        labs(title=paste('Final event', nrow(events))) +
        geom_line(aes(x=Time, y=LAeq)) +
        geom_line(aes(x=Time, y=Lma), color='magenta') +
        geom_vline(xintercept=data[idx_start,'Time'], color='blue') +
        geom_vline(xintercept=data[idx_start+idx_lmax-1,'Time'], color='red', linetype='dotted') +
        geom_vline(xintercept=data[idx_end,'Time'], color='blue') +
        geom_hline(yintercept=threshold, color='gray')
      plot(p_time)
      
      idx_start = idx_end # Split point becomes start of next event
    }
    if (length(idx_peaks)>1) {
      message('  ...finished splitting event')
    }
  }
  sec = sec + 1
}


# Plot hour in 15 min chunks ------------------------

# 
# for (hour in debug_hour) { # TODO: hours
#   data = data[data$Hour==hour,]
# 
#   L50 = LxFromLevels(data$LAeq, 50)
#   L90 = LxFromLevels(data$LAeq, 90)
#   message(paste('Hour', hour, 'L90', L90))
#   # Navy threshold is L90 + 10 of each hour +/- 30 min
#   threshold_navy = L90 + 10
# 
#   events_hour = events[events$Hour==hour,]
#   ops_hour = ops[ops$Hour==hour,]
# 
#   for (q in 1:4) {
#     start = (q-1)*(nrow(data) / 4) + 1
#     end = start + (nrow(data) / 4) - 1
# 
#     data_q = data[start:end,]
#     events_q = events_hour[events_hour$StartTime %in% data[start:end, 'Time'],]
#     ops_q = ops_hour[ops_hour$Time %in% data[start:end, 'Time'],]
# 
#     # ma = na.omit(data.frame(Time=data_q$Time, MA=movavg(data_q$LAeq)))
#     ma = na.omit(data.frame(
#       Time=data_q$Time,
#       MA=rollmean(data_q$LAeq, 10, align='center', fill=NA)
#     ))
# 
#     # Leq time series
#     p_leq = ggplot(data_q) +
#       geom_line(aes(x=Time, y=LAeq)) +
#       # Navy automated event starts
#       geom_vline(xintercept=events_q$StartTime, color='red', linetype='dotted') +
#       # Navy flight operation
#       geom_vline(xintercept=ops_q$Time, color='blue', linetype='dashed') +
#       geom_hline(yintercept=threshold_navy, color='gray') +
#       # geom_hline(yintercept=threshold_custom, color='green') +
#       geom_line(ma, mapping=aes(x=Time, y=MA), color='magenta')
# 
#     # Spectral heatmap
#     spectrum = data_q[,c(1, 31:61)] # NOTE: 6.3 Hz starts at index 26
#     names(spectrum) = gsub('1/3 LZeq ', '', names(spectrum))
# 
#     spectrum_total = data.frame()
#     for (s in 1:nrow(spectrum)) {
#       sec = as.POSIXct(spectrum$Time[s])
#       band = rownames(t(spectrum[s,c(-1)]))
#       lzeq = unname(spectrum[s,c(-1)])
#       spectrum_sec = data.frame(
#         sec,
#         band,
#         t(lzeq)
#       )
#       rownames(spectrum_sec) = c()
#       colnames(spectrum_sec) = c('Time', 'Band', 'LZeq')
#       spectrum_total = rbind(spectrum_total, spectrum_sec)
#     }
#     spectrum_total$Band = as.character(as.numeric(spectrum_total$Band))
#     spectrum_total$Band = factor(spectrum_total$Band)
#     sorted_levels = as.character(sort(as.numeric(levels(spectrum_total$Band))))
#     spectrum_total$Band = factor(spectrum_total$Band, levels=sorted_levels)
# 
#     p_spectral = ggplot(spectrum_total, aes(x=Time, y=Band, fill=LZeq)) +
#       geom_tile() +
#       scale_fill_viridis(option='A') +
#       labs(x='Time', y='Band')
# 
#     print(p_spectral / p_leq)
#   }
# }

# Distribution of events

# Recommended max onset rate of 60 dB / sec
# https://drive.google.com/drive/u/0/search?q=Low-altitude%20overflights%20of%20fighters%20the%20risk%20of%20hearing%20loss

