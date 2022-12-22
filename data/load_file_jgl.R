### Load data from JGL spl files
source('global.R')

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
  write.csv(data_txt, file='data/file_map_jgl.csv', row.names=FALSE)
  return(data_txt)
}

# get_file_map_nps = function() {
#   return(read.csv('data/file_map_nps.csv'))
# }
# 
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
# 
# # Takes an absolute path to an SDA .XLS file
# # Returns a list of data frames, one per date present in file
# load_file_nps = function(file) {
#   # Read data from .txt file
#   data_failure = TRUE
#   tryCatch({
#     data_raw = read.csv(file)
#     data_failure = FALSE
#   }, error = function(e) {
#     warning(paste('Unable to load data -', e$message, 'in', file))
#   })
#   if (data_failure) {
#     return()
#   }
#   message(paste('Loading file', basename(file)))
#   
#   #   # Clean raw data (remove any labels and metadata)
#   #   metadata_rows = which(data_raw$Place == 'Place')
#   #   if (length(metadata_rows) > 0) data_raw = data_raw[-metadata_rows,] 
#   
#   # Validate site ID
#   if (unique(data_raw$SiteID) != get_id_from_file_nps(file)) {
#     stop('File site ID does not match measurement ID(s)!')
#   }
#   
#   # Standardize time format
#   data_raw$STime = as.POSIXct(data_raw$STime, paste(format_date,format_time), tz='UTC')
#   
#   # Subset data for desired measurements and rename time column
#   data = data_raw[, selected_columns_nps]
#   names(data)[names(data) == 'STime'] = 'Time'
#   
#   # Force data to 24-hour standardized format for each date recorded
#   results = list()
#   dates = unique(format(data$Time, format=format_date))
#   for (date in dates) {
#     data_date = data[format(data$Time, format=format_date) == date,]
#     # data_date = fit_24hr_time_window(data_date)
#     results = append(results, list(data_date))
#   }
#   return (results)
# }