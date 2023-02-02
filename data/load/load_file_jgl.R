### Load data from JGL spl files
source('global.R')
library(readxl)

get_name_from_file_jgl = function(file) {
  name = basename(file)
  name = gsub('_','', name)
  name = gsub('\\s+',' ', name)
  name = substring(name, 1, gregexpr(".xlsx", name)[[1]][1] - 1)
  name = substring(name, 1, tail(unlist(gregexpr(' ', name)), n=1) - 1)
  message(paste('got name', name))
  return(name)
}

get_date_from_file_jgl = function(file) {
  # TODO: Support dates other than 2019
  date = basename(file)
  date = substring(date, 1, gregexpr(".xlsx", date)[[1]][1] - 1)
  date = substring(date, tail(unlist(gregexpr(' ', date)), n=1) + 1, nchar(date))
  breaks = unlist(gregexpr('-', date))
  month = substring(date, 1, breaks[1]-1)
  day = substring(date, breaks[1]+1, breaks[2]-1)
  date = format(as.Date(paste0('2019-',month,'-',day)), format=format_date)
  message(paste('got date', date))
  return(date)
}

# Scrape site names, id, and measurement dates from files and save to csv
map_files_jgl_csv = function() {
  # All 2019 xlsx files from the JGL database
  # TODO: include other years (with different file formatting)
  message('Mapping files to jgl site dates...')
  files = list.files(path='~/Desktop/PHI Project Data/JGL/Data', pattern="*.xlsx", full.names=TRUE, recursive=FALSE)
  data_txt = data.frame()
  for (file in files) {
    message(paste0('Mapping file ',file,'...'))
    date = get_date_from_file_jgl(file)
    name = get_name_from_file_jgl(file)
    id = get_ID_for_site_name(name)
    if (length(id) == 0) {
      warning(paste('Could not find site id for ', name, ' - skipping...'))
      next
    }
    message(paste('got id', id))
    r = data.frame(Date=date, Name=name, ID=id, File=file)
    data_txt = rbind(data_txt, r)
  }
  data_txt = cbind(Org='JGL', data_txt)
  write.csv(data_txt, file='data/load/output/file_map_jgl.csv', row.names=FALSE)
  return(data_txt)
}

get_file_map_jgl = function() {
  return(read.csv('data/load/output/file_map_jgl.csv'))
}

# # Columns to subset from the raw data
# selected_columns_nps = c(
#   'STime',
#   # A-weighted
#   'dbA',
#   # C-weighted
#   'dbC',
#   # F-weighted (TODO: assuming this is 'fast' time weighting?)
#   'dbF',
#   # Frequency content
#   'H12p5',
#   'H15p8',
#   'H20',
#   'H25',
#   'H31p5',
#   'H40',
#   'H50',
#   'H63',
#   'H80',
#   'H100',
#   'H125',
#   'H160',
#   'H200',
#   'H250',
#   'H315',
#   'H400',
#   'H500',
#   'H630',
#   'H800',
#   'H1000',
#   'H1250',
#   'H1600',
#   'H2000',
#   'H2500',
#   'H3150',
#   'H4000',
#   'H5000',
#   'H6300',
#   'H8000',
#   'H10000',
#   'H12500',
#   'H16000',
#   'H20000'
# )

# DEBUG FILE LOADING -------------------------------
file = get_file_map_jgl()[1,'File']
xlsx = readxl::read_excel(file)

# Takes an absolute path to a 2019 JGL .xlsx file, returns a list containing a data frame
# load_file_jgl = function(path) {
  
  # Read each A,B,C,... measurements page
  
  # # Read `Time History` measurements page
  # data_failure = TRUE
  # tryCatch({
  #   data_raw = as.data.frame(readxl::read_excel(path, 'Time History'))
  #   data_failure = FALSE
  # }, error = function(e) {
  #   warning(paste('Unable to load data -', e$message, 'in', path))
  # })
  # if (data_failure) {
  #   # TODO: If NAVY, scrape any pre-calculated metrics from the 'Summary' sheet
  #   return()
  # }
  # message(paste('Loading file', basename(path)))
  # 
  # # Clean raw data (remove any 'Run/Pause/Stop' metadata)
  # measurement_rows = which(is.na(data_raw$`Record Type`))
  # data = data_raw[measurement_rows,]
  # 
  # # Subset data for desired measurements
  # data = data[, selected_columns_NAVY]
  # 
  # # TODO: May want to consider using multiple time series (ts) instead of simple vectors
  # 
  # # Validate date start
  # date_start = format(data$Time[1], format=format_date)
  # if (is.na(as.Date(as.character(data$Time[1]), tz = 'UTC', format = format_date))) {
  #   # Scrape date from filename
  #   date_start_malformatted = date_start
  #   date_start = get_date_from_file_navy(path)
  #   warning(paste('Date', date_start_malformatted, 'in unexpected format. Assuming 00:00:00 start on', date_start, 'instead.'))
  #   
  #   data$Time = seq(
  #     from=as.POSIXct(paste(date_start, '00:00:00'), paste(format_date,format_time), tz='UTC'),
  #     length.out=length(data$Time),
  #     by='sec'
  #   )
  # } else if (any(date_start != format(data$Time, format=format_date))) {
  #   warning(paste('Measured dates extend beyond start date', date_start))
  # }
  # 
  # # # Validate time start
  # # time_start = format(data$Time[1], format=format_time)
  # # if (time_start != '00:00:00') {
  # #   warning(paste('Measured start time (', time_start, ') is not 00:00:00', sep=''))
  # # }
  # # 
  # # # Validate time measured (total number of seconds, assuming a 1 second frequency)
  # # time_measured = length(data$Time)
  # # 
  # # hr = floor(time_measured / 3600)
  # # min = floor((time_measured / 60) %% 60)
  # # sec = time_measured %% 60
  # # msg_time_measured = paste0('Total time measured (',hr,' hr ',min,' min ',sec,' sec)')
  # # if (time_measured < time_24hr) {
  # #   warning(paste0(msg_time_measured, ' is less than a full day'))
  # # } else if (time_measured > time_24hr) {
  # #   warning(paste0(msg_time_measured, ' is more than a full day'))
  # # }
  # 
  # # Force data to 24-hour standardized format
  # data = fit_24hr_time_window(data)
  # return (list(data))
# }