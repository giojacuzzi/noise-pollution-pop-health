### Global variables, functions, settings

library(stringr)
library(ggplot2)
library(viridis)
theme_set(theme_minimal())

format_date = '%Y-%m-%d'
format_time = '%H:%M:%S'
time_24hr = 24 * 60 * 60 # total number of seconds in a day
hours = str_pad(0:23, 2, pad = '0')
days  = c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')

get_file_map = function() {
  if (!exists('file_map')) {
    file_map = rbind(read.csv('data/load/output/file_map_navy.csv'),
                     read.csv('data/load/output/file_map_sda.csv'),
                     read.csv('data/load/output/file_map_nps.csv'))
  }
  return(file_map)
}

# NOTE: unofficial data_sites IDs produced via:
# `abbreviate(gsub(',','',data_sites[is.na(data_sites$ID),'Name']), named=F)`
get_data_sites = function() {
  if (!exists('data_sites')) {
    data_sites = read.csv('data/sites/sites.csv')
  }
  return(data_sites)
}

get_data_metrics = function() {
  if (!exists('data_metrics')) {
    data_metrics = rbind(read.csv('data/metrics/output/metrics_NAVY.csv'),
                         read.csv('data/metrics/output/metrics_NPS.csv'),
                         read.csv('data/metrics/output/metrics_SDA.csv'))
    # NOTE: Some SDA measurements were recorded with overloaded gains (i.e. distortion) that result in erroneously high values during flybys. Here, we remove site dates with measurements exceeding 110 dB.
    data_metrics = data_metrics[-which(data_metrics$Org == 'SDA' & data_metrics$Lmax > 110.0),]
    data_metrics$Date   = as.POSIXct(data_metrics$Date, tz='UTC')
    data_metrics$Day    = factor(weekdays(data_metrics$Date, abbreviate=T), levels=days)
    # data_metrics$Field  = get_field_name_for_ID(data_metrics$ID) # TODO?
    data_metrics$Period = NA
    data_metrics[data_metrics$Org=='NAVY',]$Period = get_navy_monitoring_period_for_times(data_metrics[data_metrics$Org=='NAVY',]$Date)
  }
  return(data_metrics)
}

get_data_ops = function() {
  if (!exists('data_ops')) {
    data_ops = read.csv('data/flight_ops/output/ops.csv')
    data_ops$Time   = as.POSIXct(data_ops$Time)
    data_ops$Hour   = as.factor(strftime(data_ops$Time, format='%H'))
    data_ops$DEN    = get_den_period_for_hours(data_ops$Hour)
    data_ops$Day    = factor(weekdays(data_ops$Time, abbreviate=T), levels=days)
    data_ops$Period = get_navy_monitoring_period_for_times(data_ops$Time)
  }
  return(data_ops)
}

get_field_name_for_ID = function(id) {
  data_sites = get_data_sites()
  return(data_sites[data_sites$ID==id,]$Field)
}

get_site_name_for_ID = function(id) {
  data_sites = get_data_sites()
  return(na.omit(data_sites[data_sites$ID==id,])$Name)
}

get_ID_for_site_name = function(name) {
  data_sites = get_data_sites()
  return(unique(na.omit(data_sites[data_sites$Name==name,])$ID))
}

# Day, evening, or night for hours 00-23
get_den_period_for_hours = function(h) {
  den_period = cut(as.numeric(h),
                   breaks=c(-1,6,18,21,23),
                   labels=c('Night','Day','Evening','Night'))
  return(factor(den_period, levels=c('Day','Evening','Night')))
}

get_navy_monitoring_period_for_times = function(times) {
  result = factor(months(as.POSIXct(times, tz='UTC'), abbreviate=T), levels=month.abb)
  levels(result) = c(
    '0', # Jan
    '0', # Feb
    '2', # Mar
    '2', # Apr
    '0', # May
    '3', # Jun
    '0', # Jul
    '4', # Aug
    '0', # Sep
    '0', # Oct
    '0', # Nov
    '1'  # Dec
  )
  return(result)
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