### Global variables, functions, settings
library(stringr)
library(ggplot2)
library(viridis)
library(mapview)
library(leafem)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)
library(ggpmisc)
theme_set(theme_minimal())

# Path to directory containing the PHI database
database_path = {
  # '~/../../Volumes/SAFS Backup/PHI'
  '~/../../Volumes/SAFS Work/PHI'
}

# Figure output file configuration
ggsave_output_path = 'analysis/output/'
ggsave_width = 7
ggsave_height = 6

format_date = '%Y-%m-%d'
format_time = '%H:%M:%S'
time_24hr = 24 * 60 * 60 # total number of seconds in a day
hours = str_pad(0:23, 2, pad = '0')
days  = c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')
months = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

get_file_map = function() {
  if (!exists('file_map')) {
    file_map = rbind(read.csv('data/load/output/file_map_navy.csv'),
                     read.csv('data/load/output/file_map_jgl.csv'),
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
    data_sites[data_sites==''] = NA
  }
  return(data_sites)
}

get_data_metrics = function() {
  if (!exists('data_metrics')) {
    data_metrics = rbind(read.csv('data/metrics/output/metrics_NAVY.csv'),
                         read.csv('data/metrics/output/metrics_JGL.csv'),
                         read.csv('data/metrics/output/metrics_NPS.csv'),
                         read.csv('data/metrics/output/metrics_SDA.csv'))
    # NOTE: Some SDA measurements were recorded with overloaded gains (i.e. distortion) that result in erroneously high values during flybys. Here, we remove site dates with measurements exceeding 110 dB.
    data_metrics = data_metrics[-which(data_metrics$Org == 'SDA' & data_metrics$Lmax > 110.0),]
    data_metrics$Date   = as.POSIXct(data_metrics$Date, tz='UTC')
    data_metrics$Day    = factor(weekdays(data_metrics$Date, abbreviate=T), levels=days)
    # data_metrics$Field  = get_field_name_for_ID(data_metrics$ID) # TODO?
    data_metrics$Period = NA
    data_metrics[data_metrics$Org=='NAVY',]$Period = sapply(data_metrics[data_metrics$Org=='NAVY',]$Date, get_navy_monitoring_period_for_times)
  }
  return(data_metrics)
}

get_data_ops = function() {
  if (!exists('data_ops')) {
    data_ops = read.csv('data/flight_ops/output/ops.csv')
    data_ops$Time   = as.POSIXct(data_ops$Time, tz='UTC')
    data_ops$Hour   = as.factor(format(data_ops$Time, format='%H'))
    data_ops$DEN    = get_den_period_for_hours(data_ops$Hour)
    data_ops$Day    = factor(weekdays(data_ops$Time, abbreviate=T), levels=days)
    data_ops$Date   = factor(format(data_ops$Time, format_date))
    data_ops$Period = get_navy_monitoring_period_for_times(data_ops$Time)
  }
  return(data_ops)
}

get_data_events = function() {
  if (!exists('data_events')) {
    events_jgl = read.csv('data/events/output/events_JGL.csv')
    events_jgl$Org = 'JGL'
    events_navy = read.csv('data/events/output/events_NAVY.csv')
    events_navy$Org = 'NAVY'
    events_nps = read.csv('data/events/output/events_NPS.csv')
    events_nps$Org = 'NPS'
    events_sda = read.csv('data/events/output/events_SDA.csv')
    events_sda$Org = 'SDA'
    data_events = rbind(events_jgl,
                        events_navy,
                        events_nps,
                        events_sda)
    
    # NOTE: Some SDA measurements were recorded with overloaded gains (i.e. distortion) that result in erroneously high values during flybys. Here, we remove events with measurements exceeding 110 dB.
    # data_events = data_events[-which(data_events$Org == 'SDA' & data_events$LAeq > 110.0),]
    
    data_events$TimeStart  = as.POSIXct(data_events$TimeStart, tz='UTC')
    data_events$Hour       = format(data_events$TimeStart, format='%H')
    data_events$DEN        = get_den_period_for_hours(data_events$Hour)
    data_events$Day        = factor(weekdays(data_events$TimeStart, abbreviate=T), levels=days)
    data_events$Date       = factor(format(data_events$TimeStart, format_date))
    data_events$Period     = get_navy_monitoring_period_for_times(data_events$TimeStart)
  }
  return(data_events)
}

get_data_navy_events_reported = function() {
  if (!exists('data_navy_events_reported')) {
    data_navy_events_reported = read.csv('data/events/output/navy_reported_events.csv')
    data_navy_events_reported$StartTime  = as.POSIXct(data_navy_events_reported$StartTime, tz='UTC')
    data_navy_events_reported$Hour       = format(data_navy_events_reported$StartTime, format='%H')
    data_navy_events_reported$DEN        = get_den_period_for_hours(data_navy_events_reported$Hour)
    data_navy_events_reported$Day        = factor(weekdays(data_navy_events_reported$StartTime, abbreviate=T), levels=days)
    data_navy_events_reported$Date       = factor(format(data_navy_events_reported$StartTime, format_date))
    data_navy_events_reported$Period     = get_navy_monitoring_period_for_times(data_navy_events_reported$StartTime)
  }
  return(data_navy_events_reported)
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

get_org_for_site_date = function(id, date) {
  file_map = get_file_map()
  entries_for_date = file_map[file_map$ID==id & file_map$Date==date,]
  files_for_date = entries_for_date$File
  org = unique(entries_for_date$Org)
  return(org)
}

# Day, evening, or night for hours 00-23
get_den_period_for_hours = function(h=0:23) {
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