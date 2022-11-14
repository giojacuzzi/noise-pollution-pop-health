#### NAVY noise monitoring data metric evaluation and plotting
#### Dependencies: NAVY excel spreadsheet

# Data reading -----------------------------------------------------------------
print('Reading data')

library(readxl)
# path = '~/../../Volumes/PIE/NAVY/NASWI_Site_9B_SG/NASWI - Site 9B_SG - MP1/831C_11163-20201218 000000-20121800.LD0.xlsx'
# path = '~/../../Volumes/PIE/NAVY/NASWI_Site_9B_SG/NASWI - Site 9B_SG - MP1/831C_11163-20201213 000000-20121300.RC0.xlsx'
path = '~/Desktop/831C_11163-20201218 000000-20121800.LD0.xlsx'
data_raw = as.data.frame(readxl::read_excel(path, 'Time History'))

# Data preparation -------------------------------------------------------------
print('Preparing data')

# Clean raw data (remove 'Run/Pause/Stop' metadata)
measurement_rows = which(is.na(data_raw$`Record Type`))
data = data_raw[measurement_rows,]

# Subset data for desired measurements
data = data[,c(
  'Time',
  'LAeq','LApeak',
  'LAS','LASmax',
  'LAF','LAFmax',
  'LAI','LAImax'
)]

# TODO: May want to consider using multiple time series (ts) instead of simple vectors

format_date = '%Y-%m-%d'
format_time = '%H:%M:%S'
time_24hr = 24 * 60 * 60 # total number of seconds in a day

# Validate date start
date_start = format(data$Time[1], format=format_date)
if (any(date_start != format(data$Time, format=format_date))) {
  print(paste('WARNING - measured dates extend beyond start date', date_start))
}

# Validate time start
time_start = format(data$Time[1], format=format_time)
if (time_start != '00:00:00') {
  print(paste('WARNING - measured start time (', time_start, ') is not 00:00:00', sep=''))
}

# Times for full 24hour period
Time24hr = seq(
  from=as.POSIXlt(paste(date_start, time_start), paste(format_date,format_time), tz='UTC'),
  to=as.POSIXlt(paste(date_start, '23:59:59'), paste(format_date,format_time), tz='UTC'),
  by='sec'
)
Time24hr = data.frame(Time24hr)
names(Time24hr)[names(Time24hr)=='Time24hr'] <- 'Time'
stopifnot(nrow(Time24hr)==time_24hr)

# Validate time measured (total number of seconds, assuming a 1 second frequency)
time_measured = length(data$Time)

# Left outer join to fit data within 24 hour window
data = merge(Time24hr, data, by='Time', all=TRUE) # NOTE: missing seconds will produce NAs
stopifnot(nrow(data)==time_24hr)

if (time_measured < time_24hr) {
  print(paste('WARNING - total time measured (',
              floor(time_measured / 3600),' hr ',
              floor((time_measured / 60) %% 60),' min ',
              time_measured %% 60,' sec',
              ') is less than a full day! ',
              'Calculations will ignore missing measurements.'
              , sep=''))

  # Remove NA rows from data. An alternative would be to set values to 0 (data[is.na(data)] = 0) or a moving average
  # NOTE: this will affect metric results which assume a full set of 24 hours
  data = data[rowSums(is.na(data))==0,] # Remove rows with NA values
}

# Metric Evaluation Functions --------------------------------------------------

p0 = 2*10^(-5)  # Reference sound pressure in Pascals (Pa) in air 20 μPa

# Acoustic pressure (Pa) to sound pressure level (dB)
pressureToSpl <- function(p) {
  20*log10(p/p0)
}

# Sound pressure level (dB) to acoustic pressure (Pa)
splToPressure <- function(l) {
  p0*10^(l/20)
}

# Leq is the equivalent continuous sound pressure level, also known as the "time-averaged sound pressure level". This is the steady-state sound pressure level which, over a given period of time (t_start to t_end), has the same total acoustic energy as the actual fluctuating noise signal (L). In other words, the RMS sound level with the measurement duration used as the averaging time.

# Calculate total Leq from level series over given time period (ISO 1996, Navy Technical Report)
LeqTotal <- function(L, t_start=1, t_end=length(L)) {
  duration = t_end - t_start + 1
  10*log10(sum(10^(L[t_start:t_end]/10))/duration)
}

# The sound exposure level, SEL (also referred to as LE), of a noise event is the entire event's total sound energy normalized to a constant reference time interval, typically one second. When applied to single event the SEL is called "single-event sound exposure level". SEL can be used to compare the energy of noise events which have different time durations.

# Calculate SEL from Leq of identical time period	
SelFromLeq <- function(Leq, duration) {
  Leq + 10*log10(duration) # Leq plus 10 times the log of the duration over 1 second)
}

# Calculate SEL from level series
SelFromLevels <- function(L) {
  10*log10(sum(splToPressure(L)^2) / p0^2)
}

# Exceedance levels (Lx) represent the percent of the time that was measured above a certain level. For example, an L50 of 44 dB means that for 50% of the time, the level exceeded 44 dB.
# NOTE: L100 should be equal to the min

# Calculate exceedance for the given percentage of time (0-100)
LxFromLevels <- function(L, percentage = 50) {
  if (percentage <= 0 | percentage > 100) {
    stop('Percentage must be between (0, 100]')
  }
  percentage = percentage/100.0
  ascendingLevels = L[order(L, decreasing=TRUE)]
  idx = floor(length(L) * percentage) - 1 # Exceeding, not including
  result = 0.0
  if (idx < 1) {
    result = ascendingLevels[idx+1] - 0.1
  } else {
    result = ascendingLevels[idx]
  }
  result
}

# Hourly Leq for the given interval
# TODO: make this functional for data sets without data in all 24 hours
LeqHourly <- function(Levels, Times, start, end) {
  date = format(Times[1], format=format_date)
  seconds = (Times >= as.POSIXct(paste(date,start), tz='UTC')
             & Times <= as.POSIXct(paste(date,end), tz='UTC'))
  Leqh = tapply(X=(Levels)[seconds], INDEX=cut(Times[seconds], breaks='hour'), FUN=LeqTotal)
}

# Day-night sound level, also known as DNL (ISO 1996). Returns a list including Ldn as well as intermediate calulations (Lday, Lnight, Leqh). Default level adjustment is night +10dB. United States FAA uses day values of [7am,10pm), night values of [10pm,7am)
Ldn <- function(Levels, Times) {
  Leqh_night_am = LeqHourly(Levels, Times, '00:00:00', '06:59:59') # TODO: should this pass Time24hr?
  Leqh_day      = LeqHourly(Levels, Times, '07:00:00', '21:59:59')
  Leqh_night_pm = LeqHourly(Levels, Times, '22:00:00', '23:59:59')
  Leqh_night = c(Leqh_night_am, Leqh_night_pm)
  Tday   = length(Leqh_day)
  Tnight = length(Leqh_night)
  if (Tday + Tnight != 24) {
    stop('Must provide data spanning 24 hours')
  }
  
  Lday   = LeqTotal(Leqh_day)
  Lnight = LeqTotal(Leqh_night)
  # NOTE: +10dB adjustment for night hours
  Ldn = 10*log10((Tday*10^(Lday/10) + Tnight*10^((Lnight+10)/10))/24)
  
  return(list(
    'Ldn'    = Ldn,
    'Lday'   = Lday,
    'Lnight' = Lnight,
    'Leqh'   = c(Leqh_night_am, Leqh_day, Leqh_night_pm)
  ))
}

# Day-evening-night sound level, also known as DENL (ISO 1996). Returns a list including Ldn as well as intermediate calulations (Lday, Lnight, Leqh). Default time values are day [7am,7pm), evening [7pm,10pm), and night [10pm,7am). Default level adjustments are evening +5dB, night +10dB
# NOTE: The FAA uses "Community Noise Equivalent Level" (CNEL) in California, a metric similar to Lden, however the periods are day [7am,7pm), evening [7pm,10pm) with +4.77dB adjustment, and night [10pm,7am) with +10dB adjustment.
Lden <- function(Levels, Times) {
  Leqh_night_am = LeqHourly(Levels, Times, '00:00:00', '06:59:59')
  Leqh_day      = LeqHourly(Levels, Times, '07:00:00', '18:59:59')
  Leqh_evening  = LeqHourly(Levels, Times, '19:00:00', '21:59:59')
  Leqh_night_pm = LeqHourly(Levels, Times, '22:00:00', '23:59:59')
  Leqh_night = c(Leqh_night_am, Leqh_night_pm)
  Tday     = length(Leqh_day)
  Tevening = length(Leqh_evening)
  Tnight   = length(Leqh_night)
  if (Tday + Tevening + Tnight != 24) {
    stop('Must provide data spanning 24 hours')
  }
  
  Lday     = LeqTotal(Leqh_day)
  Levening = LeqTotal(Leqh_evening)
  Lnight   = LeqTotal(Leqh_night)
  # NOTE: +5dB adjustment for evening, +10dB for night
  Lden = 10*log10((Tday*10^(Lday/10) + Tevening*10^((Levening+5)/10) + Tnight*10^((Lnight+10)/10))/24)
  
  return(list(
    'Lden'     = Lden,
    'Lday'     = Lday,
    'Levening' = Levening,
    'Lnight'   = Lnight,
    'Leqh'     = c(Leqh_night_am, Leqh_day, Leqh_evening, Leqh_night_pm)
  ))
}

# Metric Evaluation ------------------------------------------------------------
print('Evaluating metrics')

# NOTE: LAeq values are used here, but LAS/LAF/LAI could be used instead
DNL  = Ldn(data$LAeq, data$Time)
DENL = Lden(data$LAeq, data$Time)

metrics = data.frame(
  Ldn     = DNL$Ldn,
  Lden    = DENL$Lden,
  L_Aeq   = LeqTotal(data$LAeq), # Leq total from all individual Leq measurements, A-weighting
  L_ASeq  = LeqTotal(data$LAS),
  L_AFeq  = LeqTotal(data$LAF),
  L_AIeq  = LeqTotal(data$LAI),
  SEL_A   = SelFromLevels(data$LAeq),
  SEL_AS  = SelFromLevels(data$LAS),
  SEL_AF  = SelFromLevels(data$LAF),
  SEL_AI  = SelFromLevels(data$LAI),
  L_Amax  = max(data$LAeq),
  L_ASmax = max(data$LASmax),
  L_AFmax = max(data$LAFmax),
  L_AImax = max(data$LAImax),
  L_Apeak = max(data$LApeak),
  L_XAeq10 = LxFromLevels(data$LAeq, 10),
  L_XAeq25 = LxFromLevels(data$LAeq, 25),
  L_XAeq50 = LxFromLevels(data$LAeq, 50),
  L_XAeq90 = LxFromLevels(data$LAeq, 90)
)

# Plotting ---------------------------------------------------------------------
print('Plotting')

# Plot the hour containing the peak measurement
hour_peak = as.numeric(format(data[data$LApeak==max(data$LApeak),'Time'], format='%H'))
time_start = hour_peak * 3600
time_end = time_start + 3600
lp_peakhr = plot(
  data$Time[time_start:time_end],
  data$LAeq[time_start:time_end],
  main='Peak Hour Leq',
  xlab='Time (H:M)', ylab='Sound Pressure Level (dB)',
  type='l',
  ylim=c(min(data$LAeq)-5,max(data$LAeq)+5),
  xaxs='i', yaxs='i',
  xaxt='n'
)
axis.POSIXct(1, at=seq(data$Time[time_start], data$Time[time_end], by='10 min'), format='%H:%M')
points(data$Time[which(data$LAeq==max(data$LAeq))], max(data$LAeq), col='red', pch=1, cex=2.5)
abline(h=DNL$Leqh[hour_peak+1], lty='longdash')

# Plot signal metrics
bp_metrics = barplot(
  t(as.matrix(metrics)),
  main='Key Metrics',
  sub='(Raw measurements A-weighted, various time-weightings)',
  beside=TRUE,
  ylim=c(0,round(max(metrics)+20)),
  las=2,
  cex.names=0.8,
  names.arg=c(
    'Ldn','Lden',
    'Leq','Slow','Fast','Impulse',
    'SEL (Leq)','Slow','Fast','Impulse',
    'Lmax (Leq)','Slow','Fast','Impulse',
    'Lpeak','10%','25%','50%','90%'
  ),
  col=c(
    'lightskyblue', 'darkblue',
    'white','yellow1','darkorange1','firebrick3',
    'white','yellow1','darkorange1','firebrick3',
    'white','yellow1','darkorange1','firebrick3',
    'black','gray5','gray15','gray30','gray45'
  )
)
text(x=bp_metrics, y=metrics+2, labels = round(metrics,2), cex=0.5)

# Plot DNL
bp_dnl = barplot(
  DNL$Leqh,
  main='Day-night average sound level',
  xlab='Time (hr)',ylab='Leq (dB)',
  col=c(rep('darkblue',7), rep('lightskyblue',15), rep('darkblue',2)),
  ylim=c(0,round(max(DNL$Leqh)+20)),
  xaxt='n'
)
axis(1, at=bp_dnl,labels=seq(0,23))
text(x=bp_dnl, y=DNL$Leqh+2, labels=round(DNL$Leqh,1), cex=0.5)
abline(h=DNL$Ldn, lty='longdash')
text(x=1, y=DNL$Ldn+3, labels=paste(round(DNL$Ldn,2), 'dB'), cex=1.0)
abline(h=metrics$L_XAeq10, lty='dotted', col='gray')
text(x=-0.3, y=metrics$L_XAeq10, labels='10%', cex=0.5)
abline(h=metrics$L_XAeq25, lty='dotted', col='gray')
text(x=-0.3, y=metrics$L_XAeq25, labels='25%', cex=0.5)
abline(h=metrics$L_XAeq50, lty='dotted', col='gray')
text(x=-0.3, y=metrics$L_XAeq50, labels='50%', cex=0.5)
abline(h=metrics$L_XAeq90, lty='dotted', col='gray')
text(x=-0.3, y=metrics$L_XAeq90, labels='90%', cex=0.5)

print('Process finished')