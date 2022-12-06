# Plotting Functions -----------------------------------------------------------

# Plot DNL
# NOTE: Expects a list of [Ldn, Lday, Lnight, Leqh]
dnlplot = function(data) {
  bp_dnl = barplot(
    data$Leqh,
    main='Day-night average sound level',
    xlab='Time (hr)',ylab='Leq (dB)',
    col=c(rep('darkblue',7), rep('lightskyblue',15), rep('darkblue',2)),
    ylim=c(0,round(max(na.omit(data$Leqh))+20)),
    xaxt='n'
  )
  axis(1, at=bp_dnl,labels=seq(0,23))
  text(x=bp_dnl, y=data$Leqh+2, labels=round(data$Leqh,1), cex=0.5)
  abline(h=data$Ldn, lty='longdash')
  text(x=1, y=data$Ldn+3, labels=paste(round(data$Ldn,2), 'dB'), cex=1.0)
  # abline(h=metrics_A$L_XAeq10, lty='dotted', col='gray')
  # text(x=-0.3, y=metrics_A$L_XAeq10, labels='10%', cex=0.5)
  # abline(h=metrics_A$L_XAeq25, lty='dotted', col='gray')
  # text(x=-0.3, y=metrics_A$L_XAeq25, labels='25%', cex=0.5)
  # abline(h=metrics_A$L_XAeq50, lty='dotted', col='gray')
  # text(x=-0.3, y=metrics_A$L_XAeq50, labels='50%', cex=0.5)
  # abline(h=metrics_A$L_XAeq90, lty='dotted', col='gray')
  # text(x=-0.3, y=metrics_A$L_XAeq90, labels='90%', cex=0.5)
}