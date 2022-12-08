#### NAVY noise monitoring data metric evaluation and plotting
#### Dependencies: NAVY database

source('load_data.R')
source('functions_metrics.R')
source('plot.R')

# All xlsx spreadsheet files from the NAVY database
files = list.files(path='~/Desktop/NAVY Data', pattern="*.xlsx", full.names=TRUE, recursive=TRUE)

# Scrape site IDs and measurement dates from files
data_xlsx = data.frame()
for (file in files) {
  id = get_id_from_file(file)
  date = get_date_from_file(file)
  r = data.frame(ID=id, Date=date, File=file)
  data_xlsx = rbind(data_xlsx, r)
}

# Populate `data_navy` with metrics for every site ID and date
data_navy = data.frame(
  Date  = as.POSIXlt(character()),
  ID    = character(),
  Ldn   = double(),
  Lden  = double(),
  Leq   = double(),
  SEL   = double(),
  Lmax  = double(),
  Lpeak = double(),
  L10   = double(),
  L25   = double(),
  L50   = double(),
  L90   = double()
)

for (id in unique(data_xlsx$ID)) { # for every measurement site ID
  for (date in unique(data_xlsx$Date)) { # for every date at that site
    data_date = data.frame()
    files = data_xlsx[data_xlsx$ID==id & data_xlsx$Date==date, 'File']
    for (file in files) { # for every file for that date
      
      # Process the file
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
    data_date = fit_24hr_time_window(data_date) # NOTE: missing seconds will produce NAs
    if (nrow(data_date) != time_24hr) stop(paste('Error merging measurement file(s) for date', date,'-',files))
    
    # Calculate the day-night-average metrics from all data measured on that date
    DNL_A = Ldn(data_date$LAeq, data_date$Time)
    DENL_A = Lden(data_date$LAeq, data_date$Time)
    # DNL_C = Ldn(data_date$LCeq, data_date$Time)
    # DENL_C = Lden(data_date$LCeq, data_date$Time)
    
    # NOTE: Summary metrics are intended to represent the entire 24-hour period. As such, we remove any missing data to enable approximate calculations.
    if (anyNA(data_date)) {
      data_date = na.omit(data_date)
      warning('Removed NA measurements from summary metrics calculations for the time period. Leq, Lx, and SEL metrics are approximated from available data.')
    }
    
    # Calculate summary metrics for the date
    # NOTE: Primarily A-weighted and time-equalized (LAeq) values are used here, but different frequency weightings (C, Z) and time-weightings (slow, fast, impulse) can be used as well
    metrics = data.frame(
      Date  = date,
      ID    = id,
      Ldn   = DNL_A$Ldn,
      Lden  = DENL_A$Lden,
      Leq   = LeqTotal(data_date$LAeq), # Leq total from all individual Leq measurements, A-weighting
      SEL   = SelFromLevels(data_date$LAeq),
      Lmax  = max(data_date$LAeq),
      Lpeak = max(data_date$LCpeak),
      L10   = LxFromLevels(data_date$LAeq, 10),
      L25   = LxFromLevels(data_date$LAeq, 25),
      L50   = LxFromLevels(data_date$LAeq, 50),
      L90   = LxFromLevels(data_date$LAeq, 90)
    )
    
    # Add all metrics for the date to `data_navy`
    data_navy = rbind(data_navy, metrics)
    message(paste('Finished calculating', date, 'for site', id))
    # dnlplot(DNL_A, id, date)
  }
}
