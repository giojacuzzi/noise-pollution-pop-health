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
id = '24A_B'
date = '2021-08-10'
data = load_site_date(id, date)
data$Time = as.POSIXct(data$Time)
data$Hour = format(data$Time, format = '%H')

events = data_events[data_events$SiteID==id & data_events$Date==date,]
ops = data_ops[data_ops$Date==date,]

library(zoo)
# movavg = function(x, n = 5){filter(x, rep(1 / n, n), sides = 2)}
debug_hour = '20'

my_events = data.frame()

threshold_custom = 60 # Here, only look at events > 60 dB

# 10-second moving average
data$Lma = rollmean(data$LAeq, 10, align='center', fill=NA)
for (hour in debug_hour) {
  # NOTE: Navy threshold is L90 + 10 of each hour +/- 30 min 
  data_hour = data[data$Hour==hour,]
  L90 = LxFromLevels(data_hour$LAeq, 90)
  threshold = threshold_custom # Use custom threshold instead

  sec = 1
  while (sec<=nrow(data_hour)) {
    message(paste('sec',sec))
    while (is.na(data_hour$Lma[sec])) sec = sec + 1
    if (data_hour$Lma[sec] > threshold) {
      # Start event ---
      message(paste('START EVENT', data_hour$Time[sec]))
      event_start = sec
      idx_start = sec
      under_count = 0
      while (sec<=nrow(data_hour) & under_count < 5) { # < threshold for 5 sec
        while (is.na(data_hour$Lma[sec])) sec = sec + 1
        while (sec<=nrow(data_hour) & data_hour$Lma[sec] > threshold) {
          under_count = 0
          sec = sec + 1
        }
        # Below threshold again
        under_count = under_count + 1
        sec = sec + 1
      }
      # End event ---
      message(paste('END EVENT', data_hour$Time[sec]))
      event_end = sec
      idx_end = sec
      idx_local_maxima = which(ggpmisc:::find_peaks(data_hour$Lma[idx_start:idx_end])) + idx_start - 1
      # Filter only 'prominent' maxima > L25 of the event itself, and only maximums of level segments crossing over L25
      # TODO: can segments have multiple maximums??
      L25 = LxFromLevels(na.omit(data_hour[idx_start:idx_end,'Lma']), 25)
      values_over = which(data_hour[idx_start:idx_end,'Lma']>=L25) + idx_start - 1
      segments = cumsum(c(1, abs(values_over[-length(values_over)] - values_over[-1]) > 1))
      segments = by(values_over, segments, identity)
      peaks = c()
      for (segment in segments) {
        peaks = append(peaks, segment[which(data_hour[segment,'Lma']==max(data_hour[segment, 'Lma']))])
      }

      if (length(peaks) >  0) {
        idx_local_maxima = peaks
      } else {
        # If no 'prominent' maximum, take the max peak
        idx_local_maxima = idx_local_maxima[which(data_hour$Lma[idx_local_maxima]==max(data_hour$Lma[idx_local_maxima]))]
      }

      # Plot the entire event with a +/- 10 sec buffer
      buff_start = max(1, idx_start-10)
      buff_end = min(nrow(data_hour), idx_end+10)
      p_time = ggplot(data_hour[buff_start:buff_end,]) +
        labs(title=paste('Event')) +
        geom_line(aes(x=Time, y=LAeq)) +
        geom_line(aes(x=Time, y=Lma), color='magenta') +
        geom_vline(xintercept=data_hour[idx_start,'Time'], color='blue') +
        geom_vline(xintercept=data_hour[idx_end,'Time'], color='blue') +
        geom_hline(yintercept=threshold, color='gray') +
        geom_hline(yintercept=L25, color='green') +
        geom_vline(xintercept=data_hour[idx_local_maxima, 'Time'], color='orange', linetype='dotted')
      plot(p_time)
      
      # -------------------------- Multi-event
      if (length(idx_local_maxima)>1) {
        message(paste('  Splitting event into', length(idx_local_maxima)))
      }
      
        # Split multiple local maxima into separate events
        for (i in 1:length(idx_local_maxima)) {
          message(paste('maxima', i))
          if (length(idx_local_maxima)>1) {
            idx_lmax = idx_local_maxima[i]
            if (i < length(idx_local_maxima)) {
              # Find end split. It becomes next start.
              idx_end = ifelse(i<length(idx_local_maxima), idx_local_maxima[i+1], event_end)
              idx_local_min = which(ggpmisc:::find_peaks(-data_hour$Lma[idx_start:idx_end]))
              idx_end = idx_start + tail(idx_local_min, n=1) # Split point
            } else { # End of the super-event is the end of this final sub-event
              idx_end = event_end
            }
          }

          levels = data_hour$LAeq[idx_start:idx_end]
          lmax = max(levels)
          idx_lmax = which(data_hour[idx_start:idx_end,'LAeq']==lmax)[1]
          lstart = data_hour$LAeq[idx_start]
          onset = (lmax - lstart)/(idx_lmax) # dBA per sec
          event = data.frame(
            TimeStart=data_hour$Time[idx_start],
            TimeEnd=data_hour$Time[idx_end],
            Lmax=lmax,
            Onset=onset
          )
          my_events = rbind(my_events, event)
          message(paste('nrow event/my_events ', nrow(event), '/', nrow(my_events)))

          # Plot sub-events with a +/- 10 sec buffer
            buff_start = max(1, idx_start-10)
            buff_end = min(nrow(data_hour), idx_end+10)
            p_time = ggplot(data_hour[buff_start:buff_end,]) +
              labs(title=paste('Split event', nrow(my_events))) +
              geom_line(aes(x=Time, y=LAeq)) +
              geom_line(aes(x=Time, y=Lma), color='magenta') +
              geom_vline(xintercept=data_hour[idx_start,'Time'], color='blue') +
              geom_vline(xintercept=data_hour[idx_start+idx_lmax-1,'Time'], color='red', linetype='dotted') +
              geom_vline(xintercept=data_hour[idx_end,'Time'], color='blue') +
              geom_hline(yintercept=threshold, color='gray') +
              geom_hline(yintercept=LxFromLevels(data_hour[buff_start:buff_end,'LAeq'], 25), color='green') +
              geom_vline(xintercept=data_hour[idx_local_maxima, 'Time'], color='orange', linetype='dotted') +
              geom_vline(xintercept=data_hour[idx_local_min, 'Time'], color='turquoise', linetype='dotted')
            plot(p_time)

          idx_start = idx_end # Split point becomes start of next event
          if (idx_end >= event_end | idx_start >= event_end) {
            message('Overboard, breaking...')
            next
          }
        }
      # -------------------------------------------
    }
    sec = sec + 1
  }
}



# Plot hour in 15 min chunks ------------------------


for (hour in debug_hour) { # TODO: hours
  data_hour = data[data$Hour==hour,]

  L50 = LxFromLevels(data_hour$LAeq, 50)
  L90 = LxFromLevels(data_hour$LAeq, 90)
  message(paste('Hour', hour, 'L90', L90))
  # Navy threshold is L90 + 10 of each hour +/- 30 min
  threshold_navy = L90 + 10

  events_hour = events[events$Hour==hour,]
  ops_hour = ops[ops$Hour==hour,]

  for (q in 1:4) {
    start = (q-1)*(nrow(data_hour) / 4) + 1
    end = start + (nrow(data_hour) / 4) - 1

    data_q = data_hour[start:end,]
    events_q = events_hour[events_hour$StartTime %in% data_hour[start:end, 'Time'],]
    ops_q = ops_hour[ops_hour$Time %in% data_hour[start:end, 'Time'],]

    # ma = na.omit(data.frame(Time=data_q$Time, MA=movavg(data_q$LAeq)))
    ma = na.omit(data.frame(
      Time=data_q$Time,
      MA=rollmean(data_q$LAeq, 10, align='center', fill=NA)
    ))

    # Leq time series
    p_leq = ggplot(data_q) +
      geom_line(aes(x=Time, y=LAeq)) +
      # Navy automated event starts
      geom_vline(xintercept=events_q$StartTime, color='red', linetype='dotted') +
      # Navy flight operation
      geom_vline(xintercept=ops_q$Time, color='blue', linetype='dashed') +
      geom_hline(yintercept=threshold_navy, color='gray') +
      geom_hline(yintercept=threshold_custom, color='green') +
      geom_line(ma, mapping=aes(x=Time, y=MA), color='magenta')

    # Spectral heatmap
    spectrum = data_q[,c(1, 31:61)] # NOTE: 6.3 Hz starts at index 26
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

    p_spectral = ggplot(spectrum_total, aes(x=Time, y=Band, fill=LZeq)) +
      geom_tile() +
      scale_fill_viridis(option='A') +
      labs(x='Time', y='Band')

    print(p_spectral / p_leq)
  }
}



