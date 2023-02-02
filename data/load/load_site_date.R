source('global.R')
source('data/load/load_file_navy.R')
source('data/load/load_file_sda.R')
source('data/load/load_file_nps.R')

# Load data for given site ID at given date
load_site_date = function(id, date) {
  
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
    } else if (org == 'SDA') {
      data_file = load_file_sda(file)
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
    data_date = data.frame(matrix(nrow=time_24hr,ncol=2))
    data_date[1] = get_24hr_time_window(date)
    colnames(data_date) = c('Time','Value')
  }
  
  if (total_measurements != nrow(na.omit(data_date))) {
    stop(paste('Error merging measurements for date', date,'from files -', files_for_date))
  }
  
  data_date = fit_24hr_time_window(data_date) # NOTE: missing seconds will produce NAs
  if (nrow(data_date) != time_24hr) stop(paste('Error fitting data to 24-hr window for date', date))

  return(data_date)
}

# # SDA test
# id = 'KntP'
# date = '2020-07-07'
# site_date_data = load_site_date(id, date)
# site_date_dnl_metrics = LdnFromLevels(site_date_data$Value, site_date_data$Time)
# dnlplot(site_date_dnl_metrics, id, date)
# 
# # NAVY test
# id = '24A_B'
# date = '2021-06-08'
# site_date_data = load_site_date(id, date)
# site_date_dnl_metrics = LdnFromLevels(site_date_data$LAeq, site_date_data$Time)
# dnlplot(site_date_dnl_metrics, id, date)
