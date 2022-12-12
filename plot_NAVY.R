data_sites = read.csv('data/sites.csv')
data_metrics = read.csv('data/metrics/metrics_navy.csv')

data_metrics_clean = na.omit(data_metrics)

mean_dnl = tapply(data_metrics_clean$Ldn, data_metrics_clean$ID, mean)
mean_denl = tapply(data_metrics_clean$Lden, data_metrics_clean$ID, mean)
mean_day = tapply(data_metrics_clean$Lden_Lday, data_metrics_clean$ID, mean)
mean_evening = tapply(data_metrics_clean$Lden_Levening, data_metrics_clean$ID, mean)
mean_night = tapply(data_metrics_clean$Lden_Lnight, data_metrics_clean$ID, mean)

max_dnl = tapply(data_metrics_clean$Ldn, data_metrics_clean$ID, max)
max_denl = tapply(data_metrics_clean$Lden, data_metrics_clean$ID, max)
max_day = tapply(data_metrics_clean$Lden_Lday, data_metrics_clean$ID, max)
max_evening = tapply(data_metrics_clean$Lden_Levening, data_metrics_clean$ID, max)
max_night = tapply(data_metrics_clean$Lden_Lnight, data_metrics_clean$ID, max)

max_Lmax = tapply(data_metrics_clean$Lmax, data_metrics_clean$ID, max)
max_LCpeak = tapply(data_metrics_clean$LCpeak, data_metrics_clean$ID, max)

library('ggplot2')

dnl_denl = data.frame(mean_dnl, mean_denl, max_dnl, max_denl)
dnl_denl_t = data.frame(dB = c(t(dnl_denl[,])),
                   id = rep(rownames(dnl_denl),each=4),
                   metric = c('Mean Ldn','Mean Lden','Max Ldn','Max Lden'))
ggplot(dnl_denl_t, aes(fill=metric, y=dB, x=id)) + 
  geom_bar(position='dodge', stat='identity') +
  theme_minimal() + 
  labs(x='Site', y='dB', title='Day-night average means and maximums') +
  scale_fill_manual('Position', values=c('orchid3', 'royalblue', 'salmon', 'pink'))

dates_by_ldn = data_metrics_clean[with(data_metrics_clean,order(-Ldn)),]
dates_by_ldn[1:20]
