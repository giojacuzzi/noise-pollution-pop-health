#### NAVY noise monitoring data metric evaluation and plotting
#### Dependencies: NAVY excel spreadsheet

# Data reading -----------------------------------------------------------------
print('Reading data')

library(readxl)
# path = '~/../../Volumes/PIE/NAVY/NASWI_Site_9B_SG/NASWI - Site 9B_SG - MP1/831C_11163-20201218 000000-20121800.LD0.xlsx'

# NASWI Gate
path = '~/Desktop/831C_11163-20201218 000000-20121800.LD0.xlsx'
# Port Townsend City Hall
# path = '~/Desktop/831C_11164-20201218 000000-20121800.LD0.xlsx'

data_raw = as.data.frame(readxl::read_excel(path, 'Time History'))

# Data preparation -------------------------------------------------------------
print('Preparing data')

# Clean raw data (remove 'Run/Pause/Stop' metadata)
measurement_rows = which(is.na(data_raw$`Record Type`))
data = data_raw[measurement_rows,]
# names(data) = make.names(names(data))

# Subset data for desired measurements
data = data[,c(
  'Time',
  # A-weighted
  'LAeq','LApeak',
  'LAS','LASmax',
  'LAF','LAFmax',
  'LAI','LAImax',
  # C-weighted
  'LCeq','LCpeak',
  'LCS','LCSmax',
  'LCF','LCFmax',
  'LCI','LCImax',
  # Z-weighted
  'LZeq','LZpeak',
  'LZS','LZSmax',
  'LZF','LZFmax',
  'LZI','LZImax',
  # Frequency content
  # 'X1.3.LZeq.20.0',
  # 'X1.3.LZeq.80.0',
  # 'X1.3.LZeq.315',
  # 'X1.3.LZeq.1250',
  # 'X1.3.LZeq.5000',
  # 'X1.3.LZeq.20000'
  '1/3 LZeq 6.3',
  '1/3 LZeq 8.0',
  '1/3 LZeq 10.0',
  '1/3 LZeq 12.5',
  '1/3 LZeq 16.0',
  '1/3 LZeq 20.0',
  '1/3 LZeq 25.0',
  '1/3 LZeq 31.5',
  '1/3 LZeq 40.0',
  '1/3 LZeq 50.0',
  '1/3 LZeq 63.0',
  '1/3 LZeq 80.0',
  '1/3 LZeq 100',
  '1/3 LZeq 125',
  '1/3 LZeq 160',
  '1/3 LZeq 200',
  '1/3 LZeq 250',
  '1/3 LZeq 315',
  '1/3 LZeq 400',
  '1/3 LZeq 500',
  '1/3 LZeq 630',
  '1/3 LZeq 800',
  '1/3 LZeq 1000',
  '1/3 LZeq 1250',
  '1/3 LZeq 1600',
  '1/3 LZeq 2000',
  '1/3 LZeq 2500',
  '1/3 LZeq 3150',
  '1/3 LZeq 4000',
  '1/3 LZeq 5000',
  '1/3 LZeq 6300',
  '1/3 LZeq 8000',
  '1/3 LZeq 10000',
  '1/3 LZeq 12500',
  '1/3 LZeq 16000',
  '1/3 LZeq 20000'
)]

# TODO: May want to consider using multiple time series (ts) instead of simple vectors

# TODO: Integrate octave bands

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
  time_start = '00:00:00'
}

# Times for full 24hour period
Time24hr = seq(
  from=as.POSIXlt(paste(date_start, time_start), paste(format_date,format_time), tz='UTC'),
  to=as.POSIXlt(paste(date_start, '23:59:59'), paste(format_date,format_time), tz='UTC'),
  by='sec'
)
Time24hr = data.frame(Time24hr)
names(Time24hr)[names(Time24hr)=='Time24hr'] = 'Time'
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
pressureToSpl = function(p) {
  20*log10(p/p0)
}

# Sound pressure level (dB) to acoustic pressure (Pa)
splToPressure = function(l) {
  p0*10^(l/20)
}

# Leq is the equivalent continuous sound pressure level, also known as the "time-averaged sound pressure level". This is the steady-state sound pressure level which, over a given period of time (t_start to t_end), has the same total acoustic energy as the actual fluctuating noise signal (L). In other words, the RMS sound level with the measurement duration used as the averaging time.

# Calculate total Leq from level series over given time period (ISO 1996, Navy Technical Report)
LeqTotal = function(L, t_start=1, t_end=length(L)) {
  duration = t_end - t_start + 1
  10*log10(sum(10^(L[t_start:t_end]/10))/duration)
}

# The sound exposure level, SEL (also referred to as LE), of a noise event is the entire event's total sound energy normalized to a constant reference time interval, typically one second. When applied to single event the SEL is called "single-event sound exposure level". SEL can be used to compare the energy of noise events which have different time durations.

# Calculate SEL from Leq of identical time period	
SelFromLeq = function(Leq, duration) {
  Leq + 10*log10(duration) # Leq plus 10 times the log of the duration over 1 second)
}

# Calculate SEL from level series
SelFromLevels = function(L) {
  10*log10(sum(splToPressure(L)^2) / p0^2)
}

# Exceedance levels (Lx) represent the percent of the time that was measured above a certain level. For example, an L50 of 44 dB means that for 50% of the time, the level exceeded 44 dB.
# NOTE: L100 should be equal to the min

# Calculate exceedance for the given percentage of time (0-100)
LxFromLevels = function(L, percentage = 50) {
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
LeqHourly = function(Levels, Times, start, end) {
  date = format(Times[1], format=format_date)
  seconds = (Times >= as.POSIXct(paste(date,start), tz='UTC')
             & Times <= as.POSIXct(paste(date,end), tz='UTC'))
  Leqh = tapply(X=(Levels)[seconds], INDEX=cut(Times[seconds], breaks='hour'), FUN=LeqTotal)
}

# Day-night sound level, also known as DNL (ISO 1996). Returns a list including Ldn as well as intermediate calulations (Lday, Lnight, Leqh). Default level adjustment is night +10dB. United States FAA uses day values of [7am,10pm), night values of [10pm,7am)
Ldn = function(Levels, Times) {
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
Lden = function(Levels, Times) {
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

DNL_A = Ldn(data$LAeq, data$Time)
DENL_A = Lden(data$LAeq, data$Time)

# NOTE: LAeq values are used here, but LAS/LAF/LAI could be used instead
metrics_A = data.frame(
  Ldn     = DNL_A$Ldn,
  Lden    = DENL_A$Lden,
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

DNL_C = Ldn(data$LCeq, data$Time)
DENL_C = Lden(data$LCeq, data$Time)
metrics_C = data.frame(
  Ldn     = DNL_C$Ldn,
  Lden    = DENL_C$Lden,
  L_Ceq   = LeqTotal(data$LCeq),
  L_CSeq  = LeqTotal(data$LCS),
  L_CFeq  = LeqTotal(data$LCF),
  L_CIeq  = LeqTotal(data$LCI),
  SEL_C   = SelFromLevels(data$LCeq),
  SEL_CS  = SelFromLevels(data$LCS),
  SEL_CF  = SelFromLevels(data$LCF),
  SEL_CI  = SelFromLevels(data$LCI),
  L_Cmax  = max(data$LCeq),
  L_CSmax = max(data$LCSmax),
  L_CFmax = max(data$LCFmax),
  L_CImax = max(data$LCImax),
  L_Cpeak = max(data$LCpeak),
  L_XCeq10 = LxFromLevels(data$LCeq, 10),
  L_XCeq25 = LxFromLevels(data$LCeq, 25),
  L_XCeq50 = LxFromLevels(data$LCeq, 50),
  L_XCeq90 = LxFromLevels(data$LCeq, 90)
)

DNL_Z = Ldn(data$LZeq, data$Time)
DENL_Z = Lden(data$LZeq, data$Time)
metrics_Z = data.frame(
  Ldn     = DNL_Z$Ldn,
  Lden    = DENL_Z$Lden,
  L_Zeq   = LeqTotal(data$LZeq),
  L_ZSeq  = LeqTotal(data$LZS),
  L_ZFeq  = LeqTotal(data$LZF),
  L_ZIeq  = LeqTotal(data$LZI),
  SEL_Z   = SelFromLevels(data$LZeq),
  SEL_ZS  = SelFromLevels(data$LZS),
  SEL_ZF  = SelFromLevels(data$LZF),
  SEL_ZI  = SelFromLevels(data$LZI),
  L_Zmax  = max(data$LZeq),
  L_ZSmax = max(data$LZSmax),
  L_ZFmax = max(data$LZFmax),
  L_ZImax = max(data$LZImax),
  L_Zpeak = max(data$LZpeak),
  L_XZeq10 = LxFromLevels(data$LZeq, 10),
  L_XZeq25 = LxFromLevels(data$LZeq, 25),
  L_XZeq50 = LxFromLevels(data$LZeq, 50),
  L_XZeq90 = LxFromLevels(data$LZeq, 90)
)

# Plotting ---------------------------------------------------------------------
print('Plotting')
library(ggplot2)

# Plot a specific event
layout(matrix(c(1,2,3,3), 2, 2, byrow=TRUE))
time_start = 14.57 * 3600
time_end = time_start + 240
event_data = data[time_start:time_end,]
lp_event = plot(
  event_data$Time,
  event_data$LAeq,
  main='Single Event',
  xlab='Time (H:M:S)', ylab='Sound Pressure Level (dB)',
  type='l',
  ylim=c(min(event_data$LAeq)-5,max(event_data$LAeq)+5),
  xaxs='i', yaxs='i',
  xaxt='n'
)
axis.POSIXct(1, at=seq(data$Time[time_start], data$Time[time_end], by='30 sec'), format='%H:%M:%S')
Lmax = max(event_data$LAeq)
Leq = LeqTotal(event_data$LAeq)
SEL = SelFromLevels(event_data$LAeq)
Lpeak = max(event_data$LApeak)
points(event_data$Time[which(event_data$LAeq == Lmax)], Lmax, col='red', pch=1, cex=2.5)
event_metrics_A = c(Leq, SEL, Lmax, Lpeak)
abline(h=Leq, lty='longdash')
lp_metrics = barplot(
  event_metrics_A,
  main='Key Metrics',
  # sub='(Raw measurements A-weighted, various time-weightings)',
  beside=TRUE,
  ylim=c(0,round(max(event_metrics_A)+20)),
  las=2,
  cex.names=0.8,
  names.arg=c(
    'Leq','SEL','Lmax','Lpeak'
  ),
  col=c(
    'black', 'white','red','darkred'
  )
)
text(x=lp_metrics, y=event_metrics_A+4, labels = round(event_metrics_A,2), cex=0.8)
# Plot 1/3 octave bands
freq = as.matrix(event_data[,grep('1/3', names(event_data))])
freq_event = barplot(
  colMeans(freq),
  main='1/3 Octave Band Frequency Means',
  xaxt='n'
)
axis(1, at=freq_event,labels=c(6,8,10,12,16,20,25,32,40,50,63,80,100,125,160,200,250,315,400,500,630,800,1000,1250,1600,2000,2500,3150,4000,5000,6300,8000,10000,12500,16000,20000))

# Plot the hour containing the peak measurement
par(mfrow=c(1,1))
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
abline(h=DNL_A$Leqh[hour_peak+1], lty='longdash')

# Plot DNL
bp_dnl = barplot(
  DNL_A$Leqh,
  main='Day-night average sound level',
  xlab='Time (hr)',ylab='Leq (dB)',
  col=c(rep('darkblue',7), rep('lightskyblue',15), rep('darkblue',2)),
  ylim=c(0,round(max(DNL_A$Leqh)+20)),
  xaxt='n'
)
axis(1, at=bp_dnl,labels=seq(0,23))
text(x=bp_dnl, y=DNL_A$Leqh+2, labels=round(DNL_A$Leqh,1), cex=0.5)
abline(h=DNL_A$Ldn, lty='longdash')
text(x=1, y=DNL_A$Ldn+3, labels=paste(round(DNL_A$Ldn,2), 'dB'), cex=1.0)
abline(h=metrics_A$L_XAeq10, lty='dotted', col='gray')
text(x=-0.3, y=metrics_A$L_XAeq10, labels='10%', cex=0.5)
abline(h=metrics_A$L_XAeq25, lty='dotted', col='gray')
text(x=-0.3, y=metrics_A$L_XAeq25, labels='25%', cex=0.5)
abline(h=metrics_A$L_XAeq50, lty='dotted', col='gray')
text(x=-0.3, y=metrics_A$L_XAeq50, labels='50%', cex=0.5)
abline(h=metrics_A$L_XAeq90, lty='dotted', col='gray')
text(x=-0.3, y=metrics_A$L_XAeq90, labels='90%', cex=0.5)

# Plot signal metrics
bp_metrics = barplot(
  t(as.matrix(metrics_A)),
  main='Time weighting comparison (EQ, Fast, Slow, Impulse)',
  beside=TRUE,
  ylim=c(0,round(max(metrics_A)+20)),
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
text(x=bp_metrics, y=metrics_A+2, labels = round(metrics_A,2), cex=0.5)

# Plot A vs C vs Z weightings
# Metric, Value, FreqWeighting
data_by_weighting = data.frame(
  Metric=rep(c('Ldn', 'Leq', 'SEL', 'Lmax', 'Lpeak', 'Lx10', 'Lx25', 'Lx50', 'Lx90'),3),
  Value=c(
    metrics_A$Ldn, metrics_A$L_Aeq, metrics_A$SEL_A, metrics_A$L_Amax, metrics_A$L_Apeak, metrics_A$L_XAeq10, metrics_A$L_XAeq25, metrics_A$L_XAeq50, metrics_A$L_XAeq90,
    metrics_C$Ldn, metrics_C$L_Ceq, metrics_C$SEL_C, metrics_C$L_Cmax, metrics_C$L_Cpeak, metrics_C$L_XCeq10, metrics_C$L_XCeq25, metrics_C$L_XCeq50, metrics_C$L_XCeq90,
    metrics_Z$Ldn, metrics_Z$L_Zeq, metrics_Z$SEL_Z, metrics_Z$L_Zmax, metrics_Z$L_Zpeak, metrics_Z$L_XZeq10, metrics_Z$L_XZeq25, metrics_Z$L_XZeq50, metrics_Z$L_XZeq90
  ),
  FreqWeighting=c(rep('A',9),rep('C',9),rep('Z',9))
)
weights_plot = ggplot(data_by_weighting, aes(fill=FreqWeighting, y=Value, x=Metric, label=round(Value))) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(position = position_dodge2(width = 0.75, preserve = "single"), angle = 90, vjust=0.6, hjust=-0.3, size=2) +
  ggtitle('Frequency Weighting Comparison (A, C, Z)')
print(weights_plot)

print('Process finished')
