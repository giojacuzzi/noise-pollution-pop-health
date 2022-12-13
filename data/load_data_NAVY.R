### Load data from NAVY .xlsx files converted from Larson Davis binary format .LD0
source('global.R')
library(readxl)

# Columns to subset from the raw data
selected_columns_NAVY = c(
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
)

get_id_from_file = function(file) {
  id = substring(file, gregexpr("NASWI_Site_", file)[[1]][1])
  id = substring(id, 12, gregexpr("/", id)[[1]][1] - 1)
  return(id)
}

get_date_from_file = function(file) {
  date_start = substring(file, gregexpr('831C_', file)[[1]][1])
  date_start = substring(date_start, gregexpr('-', date_start)[[1]][1]+1)
  date_start = strsplit(date_start, ' ')[[1]][1]
  year = substring(date_start, 1, 4)
  month = substring(date_start, 5, 6)
  day = substring(date_start, 7, 8)
  date_start = paste(c(year,'-',month,'-',day), collapse='')
  return(date_start)
}

# Takes an absolute path to a NAVY .xlsx file, returns a data frame
load_data_NAVY = function(path) {
  # message(paste('Attempting to load', path, '...'))
  
  # Read `Time History` measurements page
  data_failure = TRUE
  tryCatch({
    data_raw = as.data.frame(readxl::read_excel(path, 'Time History'))
    data_failure = FALSE
  }, error = function(e) {
    warning(paste('Unable to load data -', e$message, 'in', path))
  })
  if (data_failure) {
    return()
  }
  
  # Clean raw data (remove any 'Run/Pause/Stop' metadata)
  measurement_rows = which(is.na(data_raw$`Record Type`))
  data = data_raw[measurement_rows,]
  
  # Subset data for desired measurements
  data = data[, selected_columns_NAVY]
  
  # TODO: May want to consider using multiple time series (ts) instead of simple vectors
  
  # Validate date start
  date_start = format(data$Time[1], format=format_date)
  if (is.na(as.Date(as.character(data$Time[1]), tz = 'UTC', format = format_date))) {
    # Scrape date from filename
    date_start_malformatted = date_start
    date_start = get_date_from_file(path)
    warning(paste('Date', date_start_malformatted, 'in unexpected format. Assuming 00:00:00 start on', date_start, 'instead.'))
    
    data$Time = seq(
      from=as.POSIXct(paste(date_start, '00:00:00'), paste(format_date,format_time), tz='UTC'),
      length.out=length(data$Time),
      by='sec'
    )
  } else if (any(date_start != format(data$Time, format=format_date))) {
    warning(paste('Measured dates extend beyond start date', date_start))
  }
  
  # Validate time start
  time_start = format(data$Time[1], format=format_time)
  if (time_start != '00:00:00') {
    warning(paste('Measured start time (', time_start, ') is not 00:00:00', sep=''))
  }
  
  # Validate time measured (total number of seconds, assuming a 1 second frequency)
  time_measured = length(data$Time)
  
  hr = floor(time_measured / 3600)
  min = floor((time_measured / 60) %% 60)
  sec = time_measured %% 60
  msg_time_measured = paste0('Total time measured (',hr,' hr ',min,' min ',sec,' sec)')
  if (time_measured < time_24hr) {
    warning(paste0(msg_time_measured, ' is less than a full day'))
  } else if (time_measured > time_24hr) {
    warning(paste0(msg_time_measured, ' is more than a full day'))
  }
  
  # Force data to 24-hour standardized format
  data = fit_24hr_time_window(data)
  return (list(data))
}