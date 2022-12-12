### Global variables and functions

format_date = '%Y-%m-%d'
format_time = '%H:%M:%S'
time_24hr = 24 * 60 * 60 # total number of seconds in a day

# Returns a data frame containing a single column, `Time`, with a row of POSIXct values for every second of the given date
get_24hr_time_window = function(date_start) {
  return(data.frame(Time=seq(
    from=as.POSIXct(paste(date_start, '00:00:00'), paste(format_date,format_time), tz='UTC'),
    to=as.POSIXct(paste(date_start, '23:59:59'), paste(format_date,format_time), tz='UTC'),
    by='sec'
  )))
}

# Fit data frame to standardized time series (by second) for a full 24 hour period
fit_24hr_time_window = function(data) {
  date_start = format(data$Time[1], format=format_date)
  
  window = get_24hr_time_window(date_start)
  
  if (length(unique(format(data$Time, format_date))) > 1) {
    warning(paste('Data extends beyond single date. Only', date_start, 'will be used.'))
  }
  if (nrow(data) > time_24hr) {
    warning(paste('Data extends beyond 24 hours. Additional rows will be discarded.'))
    data = data[1:time_24hr,]
  }
  
  data = merge(window, data, by='Time', all.x=T) # NOTE: missing seconds will produce NAs
  
  if (nrow(data) != time_24hr) stop('Error fitting data to 24-hour window')
  return(data)
}