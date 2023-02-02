# Analysis of navy flight ops record from 'Flight Operations Data'
library(stringr)
hours = str_pad(0:23, 2, pad = '0')

# Ault Field
files = list.files(path='data/flight_ops/output/ault/', pattern="*.csv", full.names=T, recursive=F)
for (file in files) {
  data = read.csv(file)
  data$LogTime = as.POSIXct(data$LogTime)
  data$Hour = strftime(data$LogTime, format='%H')
  
  numops = c()
  for (hour in hours) numops = c(numops, sum(data$Hour==hour))
  
  pdata = data.frame(
    Hour=hours,
    NumOps=numops,
    TimePeriod=cut(0:23, breaks=c(-1,6,18,22,23), labels=c('Night','Day','Evening','Night'))
  )
  
  period = substring(basename(file), 7, 9)
  ault_period = ggplot(data=pdata, aes(x=Hour, y=NumOps, fill=TimePeriod)) +
    geom_bar(stat='identity') +
    labs(title=paste('Recorded Flight Operations by Hour - Ault Field, Period', period),
         subtitle=paste('Total:', sum(numops)),
         x ='Hour',
         y ='Number of Operations') +
    scale_fill_manual(values=c('#150e5c',
                               "#abb5ff",
                               "#9c2a4b"))
  print(ault_period)
}
