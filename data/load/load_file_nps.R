### Load data from NPS spl files
source('global.R')

get_id_from_file_nps = function(file) {
  return(substring(basename(file), 7, 13))
}

get_date_from_file_nps = function(file) {
  date = substring(basename(file), 15, 24)
  date = format(gsub('_', '-', date), format=format_date)
  message(paste('File ', basename(file), date))
  return(date)
}

# Scrape site names, id, and measurement dates from files and save to csv
map_files_nps_csv = function() {
  # All txt files from the NPS database
  message('Mapping files to nps site dates...')
  files = list.files(path=paste0(database_path,'/NPS/EBLA/NVSPL'), pattern="*.txt", full.names=TRUE, recursive=TRUE)
  data_txt = data.frame()
  for (file in files) {
    message(paste0('Mapping file',file,'...'))
    date = get_date_from_file_nps(file)
    id = get_id_from_file_nps(file)
    name = get_site_name_for_ID(id)
    r = data.frame(Date=date, Name=name, ID=id, File=file)
    data_txt = rbind(data_txt, r)
  }
  data_txt = cbind(Org='NPS', data_txt)
  write.csv(data_txt, file='data/load/output/file_map_nps.csv', row.names=FALSE)
  return(data_txt)
}

get_file_map_nps = function() {
  return(read.csv('data/load/output/file_map_nps.csv'))
}

# Columns to subset from the raw data
selected_columns_nps = c(
  'STime',
  # A-weighted
  'dbA',
  # C-weighted
  'dbC',
  # F-weighted (TODO: assuming this is 'fast' time weighting?)
  'dbF',
  # Frequency content
  'H12p5',
  'H15p8',
  'H20',
  'H25',
  'H31p5',
  'H40',
  'H50',
  'H63',
  'H80',
  'H100',
  'H125',
  'H160',
  'H200',
  'H250',
  'H315',
  'H400',
  'H500',
  'H630',
  'H800',
  'H1000',
  'H1250',
  'H1600',
  'H2000',
  'H2500',
  'H3150',
  'H4000',
  'H5000',
  'H6300',
  'H8000',
  'H10000',
  'H12500',
  'H16000',
  'H20000'
)

# Takes an absolute path to an SDA .XLS file
# Returns a list of data frames, one per date present in file
load_file_nps = function(file) {
  # Read data from .txt file
  data_failure = TRUE
  tryCatch({
    data_raw = read.csv(file)
    data_failure = FALSE
  }, error = function(e) {
    warning(paste('Unable to load data -', e$message, 'in', file))
  })
  if (data_failure) {
    return()
  }
  message(paste('Loading file', basename(file)))

#   # Clean raw data (remove any labels and metadata)
#   metadata_rows = which(data_raw$Place == 'Place')
#   if (length(metadata_rows) > 0) data_raw = data_raw[-metadata_rows,] 
  
  # Validate site ID
  if (unique(data_raw$SiteID) != get_id_from_file_nps(file)) {
    stop('File site ID does not match measurement ID(s)!')
  }

  # Standardize time format
  data_raw$STime = as.POSIXct(data_raw$STime, paste(format_date,format_time), tz='UTC')

  # Subset data for desired measurements and rename time column
  data = data_raw[, selected_columns_nps]
  names(data)[names(data) == 'STime'] = 'Time'
  names(data)[names(data) == 'dbA'] = 'LAeq'
  names(data)[names(data) == 'dbC'] = 'LCeq'

  # Force data to 24-hour standardized format for each date recorded
  results = list()
  dates = unique(format(data$Time, format=format_date))
  for (date in dates) {
    data_date = data[format(data$Time, format=format_date) == date,]
    # data_date = fit_24hr_time_window(data_date)
    results = append(results, list(data_date))
  }
  return (results)
}