#### NAVY noise monitoring data metric evaluation
#### Results written to `data/metrics/metrics_navy.csv`
#### Dependencies: NAVY database

source('load_data.R')
source('functions_metrics.R')
source('plot.R')

# Present warnings immediately
options(warn = 1)

# All xlsx spreadsheet files from the NAVY database
files = list.files(path='~/Desktop/PHI Project Data/NAVY', pattern="*.xlsx", full.names=TRUE, recursive=TRUE)

# Scrape site IDs and measurement dates from files
data_xlsx = data.frame()
for (file in files) {
  id = get_id_from_file(file)
  date = get_date_from_file(file)
  r = data.frame(ID=id, Date=date, File=file)
  data_xlsx = rbind(data_xlsx, r)
}

# Populate `metrics_navy` with metrics for every site ID and date
metrics_navy = data.frame()

for (id in unique(data_xlsx$ID)) { # for every measurement site ID
  for (date in unique(data_xlsx$Date)) { # for every date at that site

    data_date = data.frame()
    files = data_xlsx[data_xlsx$ID==id & data_xlsx$Date==date, 'File']
    for (file in files) { # for every file for that date
      
      # Load the file
      data_file = load_data_NAVY(file)
      if (is.null(data_file)) {
        warning(paste('Unable to load', file, '- skipping...'))
        next
      }
      if (format(data_file$Time[1], format=format_date) != date) {
        warning(paste('Starting date of file and data are different. Will assume', date))
      }
      
      # Merge the results with any existing results for the same date
      if (nrow(data_date) == 0) {
        data_date = data_file
      } else {
        data_date = merge(na.omit(data_date), na.omit(data_file), by=names(data_file), all=TRUE)
      }
    }
    
    # If unable to load data for a date, create a representative dataframe of NAs
    if (nrow(data_date) == 0) {
      warning(paste('Unable to load data from file(s) for date', date,'-',files))
      data_date = data.frame(matrix(nrow=time_24hr,ncol=length(selected_columns)))
      data_date[1] = get_24hr_time_window(date)
      colnames(data_date) = selected_columns
      # data_date$Time = get_24hr_time_window(date)
    }
    
    data_date = fit_24hr_time_window(data_date) # NOTE: missing seconds will produce NAs
    if (nrow(data_date) != time_24hr) stop(paste('Error merging measurement file(s) for date', date,'-',files))
    
    # Calculate the day-night-average metrics from all data measured on that date
    dnl_metrics = LdnFromLevels(data_date$LAeq, data_date$Time)
    Ldn         = round(dnl_metrics$Ldn, digits=2)
    Ldn_Lday    = round(dnl_metrics$Lday, digits=2)
    Ldn_Lnight  = round(dnl_metrics$Lnight, digits=2)
    
    denl_metrics  = LdenFromLevels(data_date$LAeq, data_date$Time)
    Lden          = round(denl_metrics$Lden, digits=2)
    Lden_Lday     = round(denl_metrics$Lday, digits=2)
    Lden_Levening = round(denl_metrics$Levening, digits=2)
    Lden_Lnight   = round(denl_metrics$Lnight, digits=2)

    # NOTE: Leqh is identical for Ldn and Lden
    Leqh = round(t(unname(dnl_metrics$Leqh)), digits=2)
    Leq00 = Leqh[1]; Leq01 = Leqh[2]; Leq02 = Leqh[3]; Leq03 = Leqh[4];
    Leq04 = Leqh[5]; Leq05 = Leqh[6]; Leq06 = Leqh[7]; Leq07 = Leqh[8];
    Leq08 = Leqh[9]; Leq09 = Leqh[10]; Leq10 = Leqh[11]; Leq11 = Leqh[12];
    Leq12 = Leqh[13]; Leq13 = Leqh[14]; Leq14 = Leqh[15]; Leq15 = Leqh[16];
    Leq16 = Leqh[17]; Leq17 = Leqh[18]; Leq18 = Leqh[19]; Leq19 = Leqh[20];
    Leq20 = Leqh[21]; Leq21 = Leqh[22]; Leq22 = Leqh[23]; Leq23 = Leqh[24];
    
    # Calculate summary (24hr) metrics for the date
    # NOTE: Summary metrics are intended to represent the entire 24-hour period. As such, we remove any missing data to enable approximate calculations.
    if (anyNA(data_date)) {
      data_date = na.omit(data_date)
      warning('Removed NA measurements from summary metrics calculations for the time period. Leq, Lx, and SEL metrics are approximated from available data.')
    }
    
    # NOTE: Primarily A-weighted and time-equalized (`LAeq`) values are used here, but different frequency weightings (C, Z) and time-weightings (slow, fast, impulse) could be used as well
    Leq    = round(LeqTotal(data_date$LAeq), digits=2) # Leq total (24hr)
    SEL    = ifelse(length(data_date$LAeq) > 0, round(SelFromLevels(data_date$LAeq), digits=2), NA)
    Lmax   = ifelse(length(data_date$LAeq) > 0, max(data_date$LAeq), NA)
    LCpeak = ifelse(length(data_date$LAeq) > 0, max(data_date$LCpeak), NA)
    L10    = LxFromLevels(data_date$LAeq, 10)
    L25    = LxFromLevels(data_date$LAeq, 25)
    L50    = LxFromLevels(data_date$LAeq, 50)
    L90    = LxFromLevels(data_date$LAeq, 90)
    
    metrics = data.frame(
      # Metadata
      Date = date,
      ID   = id,
      # Ldn
      Ldn,  Ldn_Lday,  Ldn_Lnight,
      # Lden
      Lden, Lden_Lday, Lden_Levening, Lden_Lnight,
      # Leq hourly
      Leq00, Leq01, Leq02, Leq03, Leq04, Leq05, Leq06, Leq07,
      Leq08, Leq09, Leq10, Leq11, Leq12, Leq13, Leq14, Leq15,
      Leq16, Leq17, Leq18, Leq19, Leq20, Leq21, Leq22, Leq23,
      # Summary (24hr)
      Leq, SEL,
      Lmax, LCpeak,
      L10, L25, L50, L90
    )
    
    # Add all metrics for the date to `metrics_navy`
    metrics_navy = rbind(metrics_navy, metrics)
    message(paste('Finished calculating', date, 'for site', id))
  }
}

# Save all metrics data to file
write.csv(metrics_navy, file='data/metrics/metrics_navy.csv', row.names=FALSE)
