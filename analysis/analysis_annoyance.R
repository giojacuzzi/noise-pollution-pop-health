# Annoyance

source('global.R')

data_metrics = get_data_metrics()

# Ldn per site -----------------------------------------------------------------
# NOTE: includes weekends

site_date_ldns = na.omit(data_metrics[,c('Org', 'Date', 'Name', 'ID', 'Ldn')])

ggplot(site_date_ldns, aes(x=reorder(ID, Ldn, FUN=median), y=Ldn, fill=Org)) + 
  geom_boxplot(alpha=0.4) +
  labs(title='Day-night-average levels per site (Whidbey Island area)', x ='Site ID', y ='Ldn (dBA)') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_hline(yintercept=65, linetype='dotted', colour='red') + # HUD and Federal Aviation Regulation Part 150 incompatible for residential land use
  geom_hline(yintercept=55, linetype='dotted', colour='red') + # EPA recommended outdoor ambient noise level
  geom_hline(yintercept=45, linetype='dotted', colour='red') # WHO Guideline 'strong' recommendation. Evidence for a relevant absolute risk of annoyance at 45 dB Lden was rated moderate quality.

# Exposure response ------------------------------------------------------------

# The percent predicted to be highly annoyed in relation to exposure to aircraft traffic noise. Based on the WHO regression equation %HA = −50.9693 + 1.0168 × Lden + 0.0072 × Lden^2 derived from the systematic review (Guski et al., 2017).
# TODO: should be only defined for Lden [40, 75]
regression_HA = function(Lden) {
  return(-50.9693 + 1.0168 * Lden + 0.0072 * Lden^2)
}

# Plot HA (highly annoyed)

# Median
median_lden = tapply(data_metrics[!is.na(data_metrics$Lden),'Lden'], data_metrics[!is.na(data_metrics$Lden),'ID'], median)
median_lden_HA = data.frame(Lden=sort(median_lden), HA=regression_HA(sort(median_lden)))

ggplot(median_lden_HA, aes(x=Lden, y=HA, label=rownames(median_lden_HA))) +
  labs(x='Lden Median (dB)', y='%HA', title='WHO - Percent Highly Annoyed (Median Lden)') +
  geom_point(col='blue') + geom_text(hjust=0, vjust=1.5, col='blue') +
  stat_function(fun=regression_HA)

# Max
max_lden = tapply(data_metrics[!is.na(data_metrics$Lden),'Lden'], data_metrics[!is.na(data_metrics$Lden),'ID'], max)
max_lden_HA = data.frame(Lden=sort(max_lden), HA=regression_HA(sort(max_lden)))

ggplot(max_lden_HA, aes(x=Lden, y=HA, label=rownames(max_lden_HA))) +
  labs(x='Lden Maximum (dB)', y='%HA', title='WHO - Percent Highly Annoyed (Maximum Lden)') +
  geom_point(col='red') + geom_text(hjust=0, vjust=1.5, col='red') +
  geom_hline(yintercept=100, linetype='dashed') + # 100% HA
  stat_function(fun=regression_HA)
