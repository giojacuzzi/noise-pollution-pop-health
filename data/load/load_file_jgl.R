### Load data from JGL spl files
source('global.R')

get_id_from_file_jgl = function(file) {
  return(substring(basename(file), 1, 4))
}

get_date_from_file_jgl = function(file) {
  date = substring(basename(file), 6)
  date = gsub('.csv', '', date)
  return(date)
}

# Scrape site names, id, and measurement dates from files and save to csv
map_files_jgl_csv = function() {
  message('Mapping files to jgl site dates...')
  files = list.files(path=paste0(database_path,'/JGL/Data'), pattern="*csv", full.names=T, recursive=F)
  data_txt = data.frame()
  for (file in files) {
    message(paste0('Mapping file ',file,'...'))
    date = get_date_from_file_jgl(file)
    id = get_id_from_file_jgl(file)
    name = get_site_name_for_ID(id)
    if (length(id) == 0) {
      warning(paste('Could not find site id for ', name, ' - skipping...'))
      next
    }
    r = data.frame(Date=date, Name=name, ID=id, File=file)
    data_txt = rbind(data_txt, r)
  }
  data_txt = cbind(Org='JGL', data_txt)
  write.csv(data_txt, file='data/load/_output/file_map_jgl.csv', row.names=FALSE)
  return(data_txt)
}

get_file_map_jgl = function() {
  return(read.csv('data/load/_output/file_map_jgl.csv'))
}


# DEBUG FILE LOADING -------------------------------
file = get_file_map_jgl()[1,'File']

# Takes an absolute path to a JGL .csv file, returns a list containing a data frame
# Takes an absolute path to an SDA .XLS file
# Returns a list of data frames, one per date present in file
load_file_jgl = function(file) {
  # Read data from .csv file
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
  
  # Standardize time format
  date = get_date_from_file_jgl(file)
  data_raw$Time = as.POSIXct(paste(date, data_raw$Time), paste(format_date, '%I:%M:%S %p'), tz='UTC')
  
  # Force data to 24-hour standardized format for each date recorded
  results = list()
  dates = unique(format(data_raw$Time, format=format_date))
  for (date in dates) {
    data_date = data_raw[format(data_raw$Time, format=format_date)==date,]
    results = append(results, list(data_date))
  }
  return (results)
}

# TEST:
# for (file in get_file_map_jgl()$File) {
#   message(paste('loading', file))
#   result = load_file_jgl(file)
#   print(head(result[[1]]))
#   print(tail(result[[1]]))
# }
