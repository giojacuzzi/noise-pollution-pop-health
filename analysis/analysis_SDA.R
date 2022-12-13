#### SDA noise monitoring data metric evaluation
#### Results written to `data/metrics/metrics_SDA.csv`
#### Dependencies: SDA database

source('data/load_data_SDA.R')
source('analysis/metrics.R')
source('plot/plot.R')

# OPTION: Present warnings immediately
options(warn = 0)

# All xls spreadsheet files from the SDA database
files = list.files(path='~/Desktop/PHI Project Data/SDA', pattern="*.XLS", full.names=TRUE, recursive=TRUE)

# Scrape site names and measurement dates from files
data_xls = data.frame()
for (file in files) {
  name = get_name_from_path(file)
  dates = get_dates_from_file(file)
  for (date in dates) {
    r = data.frame(Name=name, Date=date, File=file)
    data_xls = rbind(data_xls, r)
  }
}
if (nrow(data_xls) <= length(files)) {
  stop('Error - Files span multiple dates, expecting more xls data entries!')
}

# Save all files data to csv
write.csv(data_xls, file='data/files_sda.csv', row.names=FALSE)

# OPTION: Present warnings immediately
options(warn = 1)

# Populate `metrics_sda` with metrics for every site name and date
metrics_sda = data.frame()

for (name in unique(data_xls$Name)) { # for every measurement site name
  for (date in unique(data_xls$Date)) { # for every date at that site

    data_date = data.frame()
    files = data_xls[data_xls$Name==name & data_xls$Date==date, 'File']
    for (file in files) { # for every file for that date

      # Load the file
      data_file = load_data_SDA(file)
      if (is.null(data_file)) {
        warning(paste('Unable to load', file, '- skipping...'))
        next
      }
      
      # Take only the measurements for the date
      date_idx = which(lapply(data_file, function(x) which(format(x$Time[1], format=format_date)==format(date, format=format_date))) == 1)
      data_file = data_file[[date_idx]]

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
      data_date = data.frame(matrix(nrow=time_24hr,ncol=2))
      data_date[1] = get_24hr_time_window(date)
      colnames(data_date) = c('Time','Value')
    }

    data_date = fit_24hr_time_window(data_date) # NOTE: missing seconds will produce NAs
    if (nrow(data_date) != time_24hr) stop(paste('Error merging measurement file(s) for date', date,'-',files))

    # Calculate the day-night-average metrics from all data measured on that date
    dnl_metrics = LdnFromLevels(data_date$Value, data_date$Time)
    Ldn         = round(dnl_metrics$Ldn, digits=2)
    Ldn_Lday    = round(dnl_metrics$Lday, digits=2)
    Ldn_Lnight  = round(dnl_metrics$Lnight, digits=2)

    denl_metrics  = LdenFromLevels(data_date$Value, data_date$Time)
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

    Leq    = round(LeqTotal(data_date$Value), digits=2) # Leq total (24hr)
    SEL    = ifelse(length(data_date$Value) > 0, round(SelFromLevels(data_date$Value), digits=2), NA)
    Lmax   = ifelse(length(data_date$Value) > 0, max(data_date$Value), NA)
    LCpeak = NA # NOTE: no C-weighted measurements from SDA data
    L10    = LxFromLevels(data_date$Value, 10)
    L25    = LxFromLevels(data_date$Value, 25)
    L50    = LxFromLevels(data_date$Value, 50)
    L90    = LxFromLevels(data_date$Value, 90)

    metrics = data.frame(
      # Metadata
      Date = date,
      Name = name,
      ID   = get_ID_for_site_name(name), # NOTE: No provided ID for SDA sites
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

    # Add all metrics for the date to `metrics_sda`
    metrics_sda = rbind(metrics_sda, metrics)
    message(paste('Finished calculating', date, 'for site', name))
  }
}

# Save all metrics data to file
write.csv(metrics_sda, file='data/metrics/metrics_sda.csv', row.names=FALSE)
