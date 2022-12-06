#### NAVY noise monitoring data metric evaluation and plotting
#### Dependencies: NAVY database

source('load_data.R')
source('functions_metrics.R')
source('plot.R')

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

files = list.files(path='~/Desktop/NAVY Data', pattern="*.xlsx", full.names=TRUE, recursive=TRUE)

for (file in files) {

  id = str = substring(file, gregexpr("NASWI_Site_", file)[[1]][1])
  id = substring(id, 12, gregexpr("/", str)[[1]][1] - 1)
  
  data_file = load_data_NAVY(file)
  date = format(data_file$Time[1], format=format_date)

  # readline(prompt=paste('Loaded date', date, 'for site', id, '- Press [enter] to continue.'))
  
  DNL_A = Ldn(data_file$LAeq, data_file$Time)
  DENL_A = Lden(data_file$LAeq, data_file$Time)
  # DNL_C = Ldn(data_file$LCeq, data_file$Time)
  # DENL_C = Lden(data_file$LCeq, data_file$Time)
  
  # NOTE: Summary metrics are intended to represent the entire 24-hour period. As such, we remove any missing data to enable approximate calculations.
  if (anyNA(data_file)) {
    data_file = na.omit(data_file)
    warning('Removed NA measurements from summary metrics calculations for the time period. Leq, Lx, and SEL metrics are approximated from available data.')
  }
  
  # NOTE: Primarily A-weighted and time-equalized (LAeq) values are used here, but different frequency weightings (C, Z) and time-weightings (slow, fast, impulse) can be used as well
  metrics = data.frame(
    Date  = date,
    ID    = id,
    Ldn   = DNL_A$Ldn,
    Lden  = DENL_A$Lden,
    Leq   = LeqTotal(data_file$LAeq), # Leq total from all individual Leq measurements, A-weighting
    SEL   = SelFromLevels(data_file$LAeq),
    Lmax  = max(data_file$LAeq),
    Lpeak = max(data_file$LCpeak),
    L10   = LxFromLevels(data_file$LAeq, 10),
    L25   = LxFromLevels(data_file$LAeq, 25),
    L50   = LxFromLevels(data_file$LAeq, 50),
    L90   = LxFromLevels(data_file$LAeq, 90)
  )
  
  data_navy = rbind(data_navy, metrics)
  
  dnlplot(DNL_A, id, date)
}
