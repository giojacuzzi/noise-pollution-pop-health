#### NAVY noise monitoring data metric evaluation and plotting
#### Dependencies: NAVY database

source('load_data.R')
source('functions_metrics.R')
source('plot.R')

path = '~/Desktop/NAVY Data/NASWI_Site_9B_SG/NASWI - Site 9B_SG - MP1/831C_11163-20201218 000000-20121800.LD0.xlsx'

# files = list.files(path='~/Desktop/NAVY Data/', pattern="*.xlsx", full.names=TRUE, recursive=TRUE)

data = load_data_NAVY(path)

date_start = format(data$Time[1], format=format_date)

DNL_A = Ldn(data$LAeq, data$Time)
DENL_A = Lden(data$LAeq, data$Time)
# DNL_C = Ldn(data$LCeq, data$Time)
# DENL_C = Lden(data$LCeq, data$Time)

# NOTE: Summary metrics are intended to represent the entire 24-hour period. As such, we remove any missing data to enable approximate calculations.
if (anyNA(data)) {
  data = na.omit(data)
  warning('Removed NA measurements from summary metrics calculations for the time period. Leq, Lx, and SEL metrics are approximated from available data.')
}

# NOTE: Primarily A-weighted and time-equalized (LAeq) values are used here, but different frequency weightings (C, Z) and time-weightings (slow, fast, impulse) can be used as well
metrics = data.frame(
  Ldn     = DNL_A$Ldn,
  Lden    = DENL_A$Lden,
  L_Aeq   = LeqTotal(data$LAeq), # Leq total from all individual Leq measurements, A-weighting
  SEL_A   = SelFromLevels(data$LAeq),
  L_Amax  = max(data$LAeq),
  L_Cpeak = max(data$LCpeak),
  L_XAeq10 = LxFromLevels(data$LAeq, 10),
  L_XAeq25 = LxFromLevels(data$LAeq, 25),
  L_XAeq50 = LxFromLevels(data$LAeq, 50),
  L_XAeq90 = LxFromLevels(data$LAeq, 90)
)

dnlplot(DNL_A)
