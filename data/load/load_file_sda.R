### Load data from SDA spl files
source('global.R')

get_name_from_file_sda = function(file) {
  name = substring(file, gregexpr("SLM Data/", file)[[1]][1] + 9)
  name = substring(name, 1, gregexpr("/", name)[[1]][1] - 1)
  return(name)
}

get_number_from_file_sda = function(file) {
  number = substring(basename(file), 4)
  number = substring(number, 1, gregexpr(".XLS", number)[[1]][1] - 1)
  return(number)
}

get_dates_from_file_sda = function(file) {
  # browser()
  data_file = load_file_sda(file)
  if (is.null(data_file)) {
    warning(paste('Unable to load', file, '- cannot retrieve date'))
    return()
  }
  dates = lapply(data_file, function(x) unique(format(x$Time, format=format_date)))
  return(unlist(dates))
}

# Scrape site names, id, and measurement dates from files and save to csv
map_files_sda_csv = function() {
  # All xls spreadsheet files from the SDA database
  message('Mapping files to sda site dates...')
  files = list.files(path='~/Desktop/PHI Project Data/SDA', pattern="*.XLS", full.names=TRUE, recursive=TRUE)
  data_xls = data.frame()
  options(warn = 1) # OPTION: Present warnings immediately
  for (file in files) {
    message(paste0('Mapping file',file,'...'))
    dates = get_dates_from_file_sda(file)
    name = get_name_from_file_sda(file)
    id = get_ID_for_site_name(name)
    for (date in dates) {
      # message(paste(id, basename(file), 'contains', date))
      r = data.frame(Date=date, Name=name, ID=id, File=file)
      data_xls = rbind(data_xls, r)
    }
  }
  if (nrow(data_xls) <= length(files)) {
    stop('Error - Files span multiple dates, expecting more xls data entries!')
  }
  data_xls = cbind(Org='SDA', data_xls)
  write.csv(data_xls, file='data/load/output/file_map_sda.csv', row.names=FALSE)
  return(data_xls)
}

get_file_map_sda = function() {
  return(read.csv('data/load/output/file_map_sda.csv'))
}

# Takes an absolute path to an SDA .XLS file
# Returns a list of data frames, one per date present in file
load_file_sda = function(file) {
  # Read data from .XLS file
  data_failure = TRUE
  tryCatch({
    data_raw = read.table(file, sep = '', header=T, na.strings='')
    data_failure = FALSE
  }, error = function(e) {
    warning(paste('Unable to load data -', e$message, 'in', file))
  })
  if (data_failure) {
    return()
  }
  message(paste('Loading file', basename(file)))
  
  # Clean raw data (remove any labels and metadata)
  metadata_rows = which(data_raw$Place == 'Place')
  if (length(metadata_rows) > 0) data_raw = data_raw[-metadata_rows,] 
  
  # Standardize time, date, and value format
  data_raw$Date = gsub('/', '-', data_raw$Date)
  data_raw$Time = as.POSIXct(paste(data_raw$Date, data_raw$Time), paste(format_date,format_time), tz='UTC')
  data_raw$Value = as.numeric(data_raw$Value)
  
  # Subset data for desired measurements and clean
  data = data_raw[, c('Time', 'Value')]
  data = na.omit(data)
  
  # Force data to 24-hour standardized format for each date recorded
  results = list()
  dates = unique(format(data$Time, format=format_date))
  for (date in dates) {
    data_date = data[format(data$Time, format=format_date) == date,]
    data_date = fit_24hr_time_window(data_date)
    results = append(results, list(data_date))
  }
  return (results)
}