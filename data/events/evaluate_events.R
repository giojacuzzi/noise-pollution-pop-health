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

# localMaxima <- function(x) {
#   # Use -Inf instead if x is numeric (non-integer)
#   y <- diff(c(-.Machine$integer.max, x)) > 0L
#   rle(y)$lengths
#   y <- cumsum(rle(y)$lengths)
#   y <- y[seq.int(1L, length(y), 2L)]
#   if (x[[1]] == x[[2]]) {
#     y <- y[-1]
#   }
#   y
# }

library(dplyr)
locate_xtrem <- function (x, last = FALSE)
{
  # use rle to deal with duplicates
  x_rle <- rle(x)
  
  # force the first value to be identified as an extrema
  first_value <- x_rle$values[1] - x_rle$values[2]
  
  # differentiate the series, keep only the sign, and use 'rle' function to
  # locate increase or decrease concerning multiple successive values.
  # The result values is a series of (only) -1 and 1.
  #
  # ! NOTE: with this method, last value will be considered as an extrema
  diff_sign_rle <- c(first_value, diff(x_rle$values)) %>% sign() %>% rle()
  
  # this vector will be used to get the initial positions
  diff_idx <- cumsum(diff_sign_rle$lengths)
  
  # find min and max
  diff_min <- diff_idx[diff_sign_rle$values < 0]
  diff_max <- diff_idx[diff_sign_rle$values > 0]
  
  # get the min and max indexes in the original series
  x_idx <- cumsum(x_rle$lengths)
  if (last) {
    min <- x_idx[diff_min]
    max <- x_idx[diff_max]
  } else {
    min <- x_idx[diff_min] - x_rle$lengths[diff_min] + 1
    max <- x_idx[diff_max] - x_rle$lengths[diff_max] + 1
  }
  # just get number of occurences
  min_nb <- x_rle$lengths[diff_min]
  max_nb <- x_rle$lengths[diff_max]
  
  # format the result as a tibble
  bind_rows(
    tibble(Idx = min, Values = x[min], NB = min_nb, Status = "min"),
    tibble(Idx = max, Values = x[max], NB = max_nb, Status = "max")) %>%
    arrange(.data$Idx) %>%
    mutate(Last = last) %>%
    mutate_at(vars(.data$Idx, .data$NB), as.integer)
}

# x <- c(1.2,2.1,9.1,9.1,2.1,1.1,1.1,5.1,5.1,1.1)
# localMaxima(x) # 3, 8
# fdsa = locate_xtrem(x)
# unname(unlist(fdsa[fdsa$Status=='max','Idx']))
# x <- c(2.1,2.1,9.1,9.1,2.1,1.1,1.1,5.1,5.1,1.1)
# localMaxima(x) # 3, 8
# fdsa = locate_xtrem(x)
# unname(unlist(fdsa[fdsa$Status=='max','Idx']))
# x <- c(3.1,2.1,9.1,9.1,2.1,1.1,1.1,5.1,5.1,1.1)
# localMaxima(x) # 1, 3, 8
# fdsa = locate_xtrem(x)
# unname(unlist(fdsa[fdsa$Status=='max','Idx']))
# x <- c(1, 2, 2, 3, 2, 1) # 4
# localMaxima(x)
# fdsa = locate_xtrem(x)
# unname(unlist(fdsa[fdsa$Status=='max','Idx']))


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
debug_hour = '20'

my_events = data.frame()

# 10-second moving average
data$Lma = rollmean(data$LAeq, 10, align='center', fill=NA)
# data$LmaX = rollmean(data$Lma, 30, align='center', fill=NA) # Further smoothing
for (hour in debug_hour) {
  # NOTE: Navy threshold is L90 + 10 of each hour +/- 30 min 
  data_hour = data[data$Hour==hour,]
  L90 = LxFromLevels(data_hour$LAeq, 90)
  # threshold = threshold_custom # Use custom threshold instead
  threshold = L90 + 10

  sec = 1
  while (sec<=nrow(data_hour)) {
    # message(paste('sec',sec))
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
      event_end = min(nrow(data_hour), sec)
      idx_end = min(nrow(data_hour), sec)
      # idx_local_maxima = which(ggpmisc:::find_peaks(data_hour$Lma[idx_start:idx_end])) + idx_start - 1
      idx_local_maxima = locate_xtrem(data_hour$Lma[idx_start:idx_end])
      idx_local_maxima = unname(unlist(idx_local_maxima[idx_local_maxima$Status=='max','Idx']))
      idx_local_maxima = idx_local_maxima + idx_start - 1
      
      # DEBUG
      if (nrow(my_events) + 1 == 26) {
      if (length(idx_local_maxima) > 1) {
        # Plot the entire event with a +/- 10 sec buffer
        buff_start = max(1, idx_start-10)
        buff_end = min(nrow(data_hour), idx_end+10)
        p_time = ggplot(data_hour[buff_start:buff_end,]) +
          labs(title=paste('Raw maxima event', nrow(my_events) + 1)) +
          geom_line(aes(x=Time, y=LAeq)) +
          geom_line(aes(x=Time, y=Lma), color='magenta') +
          # geom_line(aes(x=Time, y=LmaX), color='red') +
          geom_vline(xintercept=data_hour[idx_start,'Time'], color='blue') +
          geom_vline(xintercept=data_hour[idx_end,'Time'], color='blue') +
          geom_hline(yintercept=threshold, color='gray') +
          # geom_hline(yintercept=L25, color='green') +
          geom_vline(xintercept=data_hour[idx_local_maxima, 'Time'], color='yellow', linetype='dashed')
        plot(p_time)
        # readline(paste('Raw maxima event plotted.', length(idx_local_maxima), 'local maxima found. Press [enter] to continue...'))
      }
      } # DEBUG
      
      # DEBUG
      if (nrow(my_events) + 1 == 26) {
        message('herewego')
      }
      
      # TODO: Find all local maxima that are at least 10 dB greater than their lowest neighboring minima:
      # Lowest neighboring minima are the min values on the left and right between where the maxima is and it's next >= value on the left or right
      if (length(idx_local_maxima) > 1) {
        threshy = 10
        trimmed_idx_local_maxima = c()
        for (i in idx_local_maxima) { # for each local maxima
          local_max_level = data_hour[i,'Lma']
          
          if (nrow(my_events) + 1 == 26) {
            message(local_max_level)
          }
          # find the nearest local minima between it and its neighboring >= value
          l = i - 1
          # l_idx_min = i
          while (l >= idx_start & data_hour[l,'Lma'] <= local_max_level & data_hour[l,'Lma'] > (local_max_level-threshy)) {
            # if (data_hour[l,'Lma'] < data_hour[l_idx_min,'Lma']) l_idx_min = l
            l = l - 1
          }
          r = i + 1
          # r_idx_min = i
          while (r <= idx_end & data_hour[r,'Lma'] <= local_max_level & data_hour[r,'Lma'] > (local_max_level-threshy)) {
            # if (data_hour[r,'Lma'] < data_hour[r_idx_min,'Lma']) r_idx_min = r
            r = r + 1
          }
          if (nrow(my_events) + 1 == 26) {
            message(paste('l', l, 'i', i, 'r', r))
          }
          if ((local_max_level - data_hour[l,'Lma'] >= threshy) & (local_max_level - data_hour[r,'Lma'] >= threshy)) {
            trimmed_idx_local_maxima = append(trimmed_idx_local_maxima, i)
          }
        }
        if (length(trimmed_idx_local_maxima) == 0) {
          idx_local_maxima = idx_local_maxima[which(data_hour[idx_local_maxima,'Lma']==max(data_hour[idx_local_maxima,'Lma']))][1]
        } else {
          idx_local_maxima = trimmed_idx_local_maxima
        }
      }
      
      
      # SCRAP--------------------
      # # Filter only 'prominent' maxima > L25 of the event itself, and only maximums of level segments crossing over L25
      # L25 = LxFromLevels(na.omit(data_hour[idx_start:idx_end,'Lma']), 25)
      # values_over = which(data_hour[idx_start:idx_end,'Lma']>=L25) + idx_start - 1
      # segments = cumsum(c(1, abs(values_over[-length(values_over)] - values_over[-1]) > 1))
      # segments = by(values_over, segments, identity)
      # peaks = c()
      # for (segment in segments) {
      #   peak = segment[which(data_hour[segment,'Lma']==max(data_hour[segment, 'Lma']))][1]
      #   peaks = append(peaks, peak)
      # }
      peaks = idx_local_maxima

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
        labs(title=paste('Peak threshold event', nrow(my_events) + 1)) +
        geom_line(aes(x=Time, y=LAeq)) +
        geom_line(aes(x=Time, y=Lma), color='magenta') +
        # geom_line(aes(x=Time, y=LmaX), color='red') +
        geom_vline(xintercept=data_hour[idx_start,'Time'], color='blue') +
        geom_vline(xintercept=data_hour[idx_end,'Time'], color='blue') +
        geom_hline(yintercept=threshold, color='gray') +
        # geom_hline(yintercept=L25, color='green') +
        geom_vline(xintercept=data_hour[idx_local_maxima, 'Time'], color='orange', linetype='dotted')
      plot(p_time)
      
      # readline('Peak threshold event plotted. Press [enter] to continue...')
      
      # -------------------------- Multi-event
      if (length(idx_local_maxima)>1) {
        message(paste('  Splitting event into', length(idx_local_maxima)))
      }
      
        # Split multiple local maxima into separate events
        for (i in 1:length(idx_local_maxima)) {
          # message(paste('maxima', i))
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

          time_start = data_hour$Time[idx_start]
          time_end = data_hour$Time[idx_end]
          levels = data_hour$LAeq[idx_start:idx_end]
          lmax = max(levels)
          idx_lmax = which(data_hour[idx_start:idx_end,'LAeq']==lmax)[1]
          lstart = data_hour$LAeq[idx_start]
          onset = (lmax - lstart)/(idx_lmax) # dBA per sec
          onset = round(onset, 1)
          event = data.frame(
            TimeStart=time_start,
            TimeEnd=time_end,
            Duration=as.numeric(difftime(time_end, time_start, units='secs')),
            Leq=round(LeqTotal(levels),1),
            SEL=round(SelFromLevels(levels),1),
            Lmax=lmax,
            Onset=onset
          )
          my_events = rbind(my_events, event)
          message(paste('nrow event/my_events ', nrow(event), '/', nrow(my_events)))
          # if (length(idx_local_maxima)>1) {
          if (nrow(my_events) + 1 >= 26) { # DEBUG
          # Plot sub-events with a +/- 10 sec buffer
            buff_start = max(1, idx_start-10)
            buff_end = min(nrow(data_hour), idx_end+10)
            p_time = ggplot(data_hour[buff_start:buff_end,]) +
              labs(title=paste('Final event', nrow(my_events))) +
              geom_line(aes(x=Time, y=LAeq)) +
              geom_line(aes(x=Time, y=Lma), color='magenta') +
              # geom_line(aes(x=Time, y=LmaX), color='red') +
              geom_vline(xintercept=data_hour[idx_start,'Time'], color='blue') +
              geom_vline(xintercept=data_hour[idx_start+idx_lmax-1,'Time'], color='red', linetype='dotted') +
              geom_vline(xintercept=data_hour[idx_end,'Time'], color='blue') +
              geom_hline(yintercept=threshold, color='gray')
              # geom_hline(yintercept=LxFromLevels(data_hour[buff_start:buff_end,'LAeq'], 25), color='green') +
              # geom_vline(xintercept=data_hour[idx_local_maxima, 'Time'], color='orange', linetype='dotted')
            plot(p_time)
            # readline('Final event plotted. Press [enter] to continue...')
          # }
          } # DEBUG

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

# 
# for (hour in debug_hour) { # TODO: hours
#   data_hour = data[data$Hour==hour,]
# 
#   L50 = LxFromLevels(data_hour$LAeq, 50)
#   L90 = LxFromLevels(data_hour$LAeq, 90)
#   message(paste('Hour', hour, 'L90', L90))
#   # Navy threshold is L90 + 10 of each hour +/- 30 min
#   threshold_navy = L90 + 10
# 
#   events_hour = events[events$Hour==hour,]
#   ops_hour = ops[ops$Hour==hour,]
# 
#   for (q in 1:4) {
#     start = (q-1)*(nrow(data_hour) / 4) + 1
#     end = start + (nrow(data_hour) / 4) - 1
# 
#     data_q = data_hour[start:end,]
#     events_q = events_hour[events_hour$StartTime %in% data_hour[start:end, 'Time'],]
#     ops_q = ops_hour[ops_hour$Time %in% data_hour[start:end, 'Time'],]
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

