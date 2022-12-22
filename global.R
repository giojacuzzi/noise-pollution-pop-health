### Global variables and functions

format_date = '%Y-%m-%d'
format_time = '%H:%M:%S'
time_24hr = 24 * 60 * 60 # total number of seconds in a day

# NOTE: unofficial data_sites IDs produced via:
# `abbreviate(gsub(',','',data_sites[is.na(data_sites$ID),'Name']), named=F)`
get_data_sites = function() {
  if (!exists('data_sites')) {
    return(read.csv('data/sites.csv'))
  }
  return(data_sites)
}

get_file_map = function() {
  if (!exists('file_map')) {
    return(rbind(read.csv('data/file_map_navy.csv'),
                 read.csv('data/file_map_sda.csv'),
                 read.csv('data/file_map_nps.csv')))
  }
  return(file_map)
}

get_data_metrics = function() {
  if (!exists('data_metrics')) {
    return(rbind(read.csv('data/metrics/metrics_NAVY.csv'),
                 read.csv('data/metrics/metrics_NPS.csv'),
                 read.csv('data/metrics/metrics_SDA.csv')))
  }
  return(data_metrics)
}

get_site_name_for_ID = function(id) {
  data_sites = get_data_sites()
  return(na.omit(data_sites[data_sites$ID==id,])$Name)
}

get_ID_for_site_name = function(name) {
  data_sites = get_data_sites()
  return(unique(na.omit(data_sites[data_sites$Name==name,])$ID))
}

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