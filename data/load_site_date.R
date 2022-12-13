source('global.R')
source('plot/plot.R')
source('data/load_data_SDA.R')
source('analysis/metrics.R')

data_files_navy = read.csv('data/files_navy.csv')
data_files_navy = cbind(Org='NAVY', data_files_navy)
data_files_sda = read.csv('data/files_sda.csv')
data_files_sda = cbind(Org='SDA', data_files_sda)
data_files = rbind(data_files_navy, data_files_sda)

data_metrics_navy = read.csv('data/metrics/metrics_navy.csv')
data_metrics_navy = cbind(Org='NAVY', data_metrics_navy)
data_metrics_sda = read.csv('data/metrics/metrics_sda.csv')
data_metrics_sda = cbind(Org='SDA', data_metrics_sda)
data_metrics = rbind(data_metrics_navy, data_metrics_sda)

load_site_date = function(id, date) {
  
  entries_for_date = data_files[data_files$ID==id & data_files$Date==date,]
  files_for_date = entries_for_date$File
  org = unique(entries_for_date$Org)
  if (length(files_for_date) == 0) {
    error(paste('Could note find site date', id, date))
  }
  
  message(paste0('Loading ', id, ' ', date, ' (', length(files_for_date), ' files)...'))
  
  data_date = data.frame()
  total_measurements = 0
  for (file in files_for_date) {
    
    if (org == 'NAVY') {
      error('TODO: Unsupported org data!')
    } else if (org == 'SDA') {
      
      # Load the file
      data_file = load_data_SDA(file)
      if (is.null(data_file)) {
        warning(paste('Unable to load', file, '- skipping...'))
        next
      }
      # Take only the measurements for the date
      date_idx = which(lapply(data_file, function(x) which(format(x$Time[1], format=format_date)==format(date, format=format_date))) == 1)
      data_file = data_file[[date_idx]]
      
      total_measurements = nrow(na.omit(data_file)) + total_measurements
      
      # Merge the results with any existing results for the same date
      if (nrow(data_date) == 0) {
        data_date = data_file
      } else {
        data_date = merge(na.omit(data_date), na.omit(data_file), by=names(data_file), all=TRUE)
      }

    } else {
      error('Unsupported org data!')
    }
  }

  # If unable to load data for a date, create a representative dataframe of NAs
  if (nrow(data_date) == 0) {
    warning(paste('Unable to load data from file(s) for date', date,'-',files))
    data_date = data.frame(matrix(nrow=time_24hr,ncol=2))
    data_date[1] = get_24hr_time_window(date)
    colnames(data_date) = c('Time','Value')
  }
  
  if (total_measurements != nrow(na.omit(data_date))) {
    error('Error merging data - total measurements inconsistent')
  }
  
  data_date = fit_24hr_time_window(data_date) # NOTE: missing seconds will produce NAs
  if (nrow(data_date) != time_24hr) stop(paste('Error merging measurement file(s) for date', date,'-',files))
  
  return(data_date)
}

# SDA test
id = 'KntP'
date = '2020-07-07'
site_date_data = load_site_date(id, date)
site_date_dnl_metrics = LdnFromLevels(site_date_data$Value, site_date_data$Time)
dnlplot(site_date_dnl_metrics, id, date)

# NAVY test
id = '24A_B'
date = '2021-06-08'
site_date_data = load_site_date(id, date)
site_date_dnl_metrics = LdnFromLevels(site_date_data$Value, site_date_data$Time)
dnlplot(site_date_dnl_metrics, id, date)
