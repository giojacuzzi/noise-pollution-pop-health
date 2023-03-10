# Histograms of noise event LAeq_Lmax distribution (per-site, per-period)

library(ggplot2)
library(patchwork)
theme_set(theme_minimal())

sites = read.csv('data/sites/sites.csv')
sites = sites[sites$Org=='NAVY' & (sites$Region=='Ault Field' | sites$Region=='OLF Coupeville'), ]
events = read.csv('data/events/output/events.csv')

plots_events_per_site = list()
for (s in 1:nrow(sites)) {
  site = sites[s,'ID']
  events_site = events[events$SiteID==site,]
  
  # Consider only events above 85 dB
  events_site = events_site[events_site$LAeq_Lmax>=75.0,]
  
  events_site$Period = as.factor(events_site$Period)
  mean_events = nrow(events_site)/4
  
  p = ggplot(events_site, aes(x=LAeq_Lmax, fill=Period)) +
    geom_histogram(binwidth=5, boundary=-2.5, position='identity', alpha = 0.5) +
    # xlim(25, 125) + ylim(0,1500) +
    labs(title=paste0(site, ' (', sites[s,'Name'], ')'),
         subtitle=paste('Mean events per week:', mean_events)) +
    labs(x =ifelse((s==4 | s==9),'LAeq_Lmax',''),
         y =ifelse((s==4 | s==9),'Number of events','')) +
    theme(legend.position=ifelse((s==5 | s==11), 'right', 'none'))
  
  plots_events_per_site[[s]] = p
}

# Ault
print(
  (plots_events_per_site[[1]] + plots_events_per_site[[2]] + plots_events_per_site[[3]]) / (plots_events_per_site[[4]] + plots_events_per_site[[5]] + plot_spacer())
)

# Coup
print(
  (plots_events_per_site[[6]] + plots_events_per_site[[7]] + plots_events_per_site[[8]]) / (plots_events_per_site[[9]] + plots_events_per_site[[10]] + plots_events_per_site[[11]])
)                        
