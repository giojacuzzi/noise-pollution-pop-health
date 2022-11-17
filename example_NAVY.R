#### NAVY noise monitoring data metric evaluation and plotting
#### Dependencies: NAVY excel spreadsheet

source('load_data.R')
source('metric_analysis.R')

# Data reading -----------------------------------------------------------------

# path = '~/../../Volumes/PIE/NAVY/NASWI_Site_9B_SG/NASWI - Site 9B_SG - MP1/831C_11163-20201218 000000-20121800.LD0.xlsx'

# NASWI Gate
path = '~/Desktop/831C_11163-20201218 000000-20121800.LD0.xlsx'
# Port Townsend City Hall
# path = '~/Desktop/831C_11164-20201218 000000-20121800.LD0.xlsx'

data = load_data_NAVY(path)

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

