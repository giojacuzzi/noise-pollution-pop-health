library(readxl)

# Global variables
format_date = '%Y-%m-%d'
format_time = '%H:%M:%S'

# Takes an absolute path to a NAVY .xlsx file converted from Larson Davis binary format .LD0
# Returns a data frame
load_data_NAVY = function(path) {
  message(paste('Attempting to load', path, '...'))
  
  # Read Time History measurements page
  data_raw = as.data.frame(readxl::read_excel(path, 'Time History'))
  
  # Clean raw data (remove any 'Run/Pause/Stop' metadata)
  measurement_rows = which(is.na(data_raw$`Record Type`))
  data = data_raw[measurement_rows,]
  
  # Subset data for desired measurements
  data = data[,c(
    'Time',
    # A-weighted
    'LAeq','LApeak',
    'LAS','LASmax',
    'LAF','LAFmax',
    'LAI','LAImax',
    # C-weighted
    'LCeq','LCpeak',
    'LCS','LCSmax',
    'LCF','LCFmax',
    'LCI','LCImax',
    # Z-weighted
    'LZeq','LZpeak',
    'LZS','LZSmax',
    'LZF','LZFmax',
    'LZI','LZImax',
    # Frequency content
    '1/3 LZeq 6.3',
    '1/3 LZeq 8.0',
    '1/3 LZeq 10.0',
    '1/3 LZeq 12.5',
    '1/3 LZeq 16.0',
    '1/3 LZeq 20.0',
    '1/3 LZeq 25.0',
    '1/3 LZeq 31.5',
    '1/3 LZeq 40.0',
    '1/3 LZeq 50.0',
    '1/3 LZeq 63.0',
    '1/3 LZeq 80.0',
    '1/3 LZeq 100',
    '1/3 LZeq 125',
    '1/3 LZeq 160',
    '1/3 LZeq 200',
    '1/3 LZeq 250',
    '1/3 LZeq 315',
    '1/3 LZeq 400',
    '1/3 LZeq 500',
    '1/3 LZeq 630',
    '1/3 LZeq 800',
    '1/3 LZeq 1000',
    '1/3 LZeq 1250',
    '1/3 LZeq 1600',
    '1/3 LZeq 2000',
    '1/3 LZeq 2500',
    '1/3 LZeq 3150',
    '1/3 LZeq 4000',
    '1/3 LZeq 5000',
    '1/3 LZeq 6300',
    '1/3 LZeq 8000',
    '1/3 LZeq 10000',
    '1/3 LZeq 12500',
    '1/3 LZeq 16000',
    '1/3 LZeq 20000'
  )]
  
  # TODO: May want to consider using multiple time series (ts) instead of simple vectors
  
  time_24hr = 24 * 60 * 60 # total number of seconds in a day
  
  # Validate date start
  date_start = format(data$Time[1], format=format_date)
  if (any(date_start != format(data$Time, format=format_date))) {
    warning(paste('Measured dates extend beyond start date', date_start))
  }
  
  # Validate time start
  time_start = format(data$Time[1], format=format_time)
  if (time_start != '00:00:00') {
    warning(paste('Measured start time (', time_start, ') is not 00:00:00', sep=''))
    time_start = '00:00:00'
  }
  
  # Times for full 24hour period
  Time24hr = seq(
    from=as.POSIXlt(paste(date_start, time_start), paste(format_date,format_time), tz='UTC'),
    to=as.POSIXlt(paste(date_start, '23:59:59'), paste(format_date,format_time), tz='UTC'),
    by='sec'
  )
  Time24hr = data.frame(Time24hr)
  names(Time24hr)[names(Time24hr)=='Time24hr'] = 'Time'
  stopifnot(nrow(Time24hr)==time_24hr)
  
  # Validate time measured (total number of seconds, assuming a 1 second frequency)
  time_measured = length(data$Time)
  
  # Left outer join to fit data within 24 hour window
  data = merge(Time24hr, data, by='Time', all=TRUE) # NOTE: missing seconds will produce NAs
  stopifnot(nrow(data)==time_24hr)
  
  if (time_measured < time_24hr) {
    warning(paste('Total time measured (',
                floor(time_measured / 3600),' hr ',
                floor((time_measured / 60) %% 60),' min ',
                time_measured %% 60,' sec',
                ') is less than a full day. ',
                # 'Calculations will ignore missing measurements.',
                sep=''))
    
    # Remove NA rows from data. An alternative would be to set values to 0 (data[is.na(data)] = 0) or a moving average
    # NOTE: this will affect metric results which assume a full set of 24 hours
    # data = data[rowSums(is.na(data))==0,] # Remove rows with NA values
  }
  
  return (data)
}