## Utility functions for loading acoustic monitoring data from a given site and date

source('global.R')
source('data/load/load_file_navy.R')
source('data/load/load_file_jgl.R')
source('data/load/load_file_nps.R')

# Load data for given site ID at given date from converted csv (fast)
load_site_date = function(id, date) {
  date = as.POSIXct(date, tz='UTC')
  org = get_org_for_site_date(id, date)
  file = paste0(here::here(), '/data/load/_output/site_dates/', org, '/', id, '_', date, '.csv')
  if (!file.exists(file)) {
    warning(paste('File does not exist:', file))
    return(NULL)
  }
  
  data_date = NULL
  data_date = read_csv(file, show_col_types = F)
  if (is.null(data_date)) {
    warning(paste('Unable to load', file))
    return(NULL)
  }
  
  # If unable to load data for a date, create a representative dataframe of NAs
  if (nrow(data_date) == 0) {
    warning(paste('Unable to load data for date', date))
    data_date = data.frame(matrix(nrow=time_24hr,ncol=1))
    data_date[1] = get_24hr_time_window(date)
    colnames(data_date) = c('Time')
    return(data_date)
  }
  
  data_date$Time = as.POSIXct(data_date$Time, paste(format_date,format_time), tz='UTC')
  
  data_date = fit_24hr_time_window(data_date) # NOTE: missing seconds will produce NAs
  if (nrow(data_date) != time_24hr) stop(paste('Error fitting data to 24-hr window for date', date))
  
  return(data_date)
}



# Load data for given site ID at given date from raw database files (slow)
load_site_date_raw = function(id, date) {
  
  file_map = get_file_map()
  
  entries_for_date = file_map[file_map$ID==id & file_map$Date==date,]
  files_for_date = entries_for_date$File
  org = unique(entries_for_date$Org)
  if (length(files_for_date) == 0) {
    stop(paste('Could not find site date', id, date))
  }
  
  message(paste0('Loading ', id, ' ', date, ' (', length(files_for_date), ' files)...'))
  
  data_date = data.frame()
  total_measurements = 0
  data_file = NULL
  for (file in files_for_date) {

    # Load the file data
    if (org == 'NAVY') {
      data_file = load_file_navy(file)
    } else if (org == 'JGL') {
      data_file = load_file_jgl(file)
    } else if (org == 'NPS') {
      data_file = load_file_nps(file)
    } else {
      stop('Unsupported org data!')
    }
    if (is.null(data_file)) {
      warning(paste('Unable to load', file, '- skipping...'))
      next
    }
    
    # Take only the measurements for the date
    date_idx = which(lapply(data_file, function(x) which(format(x$Time[1], format=format_date)==format(date, format=format_date))) == 1)
    data_file = data_file[[date_idx]]
    total_measurements = nrow(na.omit(data_file)) + total_measurements
    
    # Merge results with any existing results for the same date
    if (nrow(data_date) == 0) {
      data_date = data_file
    } else {
      data_date = merge(na.omit(data_date), na.omit(data_file), by=names(data_file), all=TRUE)
    }
  }

  # If unable to load data for a date, create a representative dataframe of NAs
  if (nrow(data_date) == 0) {
    warning(paste('Unable to load data for date', date,'from files -', files_for_date))
    data_date = data.frame(matrix(nrow=time_24hr,ncol=1))
    data_date[1] = get_24hr_time_window(date)
    colnames(data_date) = c('Time')
    return(data_date)
  }
  
  if (total_measurements != nrow(na.omit(data_date))) {
    stop(paste('Error merging measurements for date', date,'from files -', files_for_date))
  }
  
  data_date = fit_24hr_time_window(data_date) # NOTE: missing seconds will produce NAs
  if (nrow(data_date) != time_24hr) stop(paste('Error fitting data to 24-hr window for date', date))

  return(data_date)
}

create_site_date_csvs = function(orgarg) {
  
  options(warn = 1)
  file_map = get_file_map()
  if (orgarg != '') {
    orgarg = toupper(orgarg)
    file_map = file_map[file_map$Org==orgarg,]
  }

  num_processed = 0
  for (id in unique(file_map$ID)) { # for every site ID
    
    id = as.character(id)
    num_processed = num_processed + 1
    name = get_site_name_for_ID(id)
    message(paste0('Processing site ', id, ' \"', name , '\" - ', num_processed, ' of ', length(unique(file_map$ID))))
    
    dates = unique(file_map[file_map$ID==id, 'Date'])
    for (d in 1:length(dates)) { # for every date at that site
      
      date = dates[d]
      site_date_data = load_site_date_raw(id, date)
      org = unique(file_map[file_map$ID==id & file_map$Date==date,]$Org)
      site_date_levels = site_date_data$LAeq
      if (is.null(site_date_levels)) {
        warning(paste('Unable to get levels for', id, date, '- skipping...'))
        next
      }
      
      # Save all site_dates data to csv
      path = paste0(here::here(), '/data/load/_output/site_dates/', org)
      if (!dir.exists(path)) dir.create(path, recursive=T)
      path = paste0(path, '/', id, '_', date, '.csv')
      write.csv(site_date_data[!is.na(site_date_data$LAeq),], file=path, row.names=F)
      message(paste('Wrote', path))
    }
  }
}
