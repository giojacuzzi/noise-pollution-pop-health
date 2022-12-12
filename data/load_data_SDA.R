### Load data from SDA spl files
source('global.R')

get_name_from_file = function(file) {
  name = substring(file, gregexpr("SLM Data/", file)[[1]][1] + 9)
  name = substring(name, 1, gregexpr("/", name)[[1]][1] - 1)
  return(name)
}

get_number_from_file = function(file) {
  number = substring(basename(file), 4)
  number = substring(number, 1, gregexpr(".XLS", number)[[1]][1] - 1)
  return(number)
}

# Takes an absolute path to an SDA .XLS file
# Returns a data frame
load_data_SDA = function(path) {
  message(paste('Attempting to load', path, '...'))

  # Read `Time History` measurements page
  data_failure = TRUE
  tryCatch({
    data_raw = read.table(file, sep = '', header=T, na.strings='')
    data_failure = FALSE
  }, error = function(e) {
    warning(paste('Unable to load data -', e$message, 'in', path))
  })
  if (data_failure) {
    return()
  }
  
  # Standardize time and date format
  data_raw$Date = gsub('/', '-', data_raw$Date)
  data_raw$Time = as.POSIXct(paste(data_raw$Date, data_raw$Time), paste(format_date,format_time), tz='UTC')
  
  # Subset data for desired measurements
  data = data_raw[, c('Time', 'Value')]
  
  # Validate date start
  date_start = format(data$Time[1], format=format_date)
  if (any(date_start != format(data$Time, format=format_date))) {
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
  return (data)
}