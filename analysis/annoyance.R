## Health and well-being impact assessment - Annoyance

source('global.R')
source('data/metrics/metrics.R')

data_sites   = get_data_sites()
data_metrics = get_data_metrics()

## Annoyance -------------------------------------------------------------------
# What is the risk of high annoyance at these sites based on exposure-response relationships?
# Dependencies: any dataset

# TODO: Annoyance on a longer, "proper" timescale? Energy averaged across the four monitoring periods?

# See ISO 1996-1 2016 Annex E/F and Lct

# ISO 1996
# (3.2.2): Long-term time interval - Specified time interval over which the sound of series of reference time intervals is averaged or assessed; The long-term time interval is determined for the purpose of describing environmental noise as it is generally designated by responsible authorities; For long-term assessments and land-use planning long-term time intervals that represent some significant fraction of year should be used (e.g 3 months, 6 months, and 1 year)
# 
# (8.1): Estimation of long-term annoyance response of communities - Noise assessments representing a long-term time interval, typically year, are used to estimate the annoyance  response of communities to the overall, steady sound situation. Annex E or Annex F (Gio: we are using Annex F, a dose-response regression) should be sued to estimate the long-term annoyance of communities to airport, road-traffic, or railroad noise. Each of these two annexes provides estimates of the percentage of a typical population that is likely to be highly annoyed by that environmental noise due to a specific annual average adjusted day-night sound level.
# 
# (D.4): Qualifications to the dose-response functions - These formulae are applicable only to long-term environmental sounds such as the yearly average; These formulae should not be used with shorter time periods such as weekends single season or busy days, rather, the annual average or some other long-term period should be used; These formulae are not applicable to short-term environmental sound such as from an increase in road traffic due to short-duration construction project; These formulae are only applicable to existing situations
# 
# Note that the WHO guidelines for Ldn/Lden/Lday/Lnight are based on this same 1996 standard –– in their words, "an average sound pressure level over all days, evenings and nights in a year ".

# The percent predicted to be highly annoyed in relation to exposure to aircraft traffic noise. Based on the WHO regression equation %HA = −50.9693 + 1.0168 × Lden + 0.0072 × Lden^2 derived from the systematic review (Guski et al., 2017).
# TODO: should be only defined for Lden [40, 75]
regression_WHO = function(Lden) {
  return(-50.9693 + 1.0168 * Lden + 0.0072 * Lden^2)
}
bounds_who = c(40,75)

# Miedema and Oudshoorn 2001
regression_MO = function(Lden) {
  return(-9.199 * 10^-5 * (Lden - 42)^3 + 3.932 * 10^-2 * (Lden - 42)^2 + 0.2939 * (Lden - 42))
}

regression_ISO_Fidell = function(Lden) {
  # Includes ISO recommended Lct of 71.3 dB to implement a 7 dB adjustment
  # 1996-1 2016 / Fidell et al 2011
  Lct = 71.3
  return(
    100 * exp(1)^(-(1/(10^(0.1*(Lden-Lct+4.7))))^0.3)
  )
}
regression_ISO_Miedema = function(Lden) {
  # Includes ISO recommended 7 dB adjustment, based on Miedema curve
  return(-9.199 * 10^-5 * (Lden - 40)^3 + 3.932 * 10^-2 * (Lden - 40)^2 + 0.294 * (Lden - 40))
}
ci_iso_miedema = data.frame(
  Lden=  c(43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76),
  Lower= c(0.3,0.4,0.4,0.5,0.6,0.7,0.9,1.0,1.2,1.4,1.7,1.9,2.2,2.6,3.0,3.4,3.9,4.4,5.0,5.7,6.4,7.2,8.1,9.0,10.0,11.1,12.3,13.6,15.0,16.4,18.0,19.6,21.3,23.1),
  Upper= c(33.5,35.7,38.0,40.3,42.7,45.1,47.5,49.9,52.3,54.7,57.1,59.5,61.8,64.1,66.3,68.5,70.6,72.7,74.7,76.6,78.4,80.1,81.8,83.4,84.8,86.2,87.5,88.7,89.9,90.9,91.9,92.7,93.6,94.3)
)
bounds_iso_miedema = c(40,76)

# Yokoshima et al 2021
regression_japan = function(Lden) {
  return(-68.080 + 1.838 * Lden + 0.006 * Lden^2) # (R^2 = 0.994)
}
ci_japan = data=data.frame(
  Lden=  c(40,   45,  50,  55, 60, 65),
  Lower= c(8.1,  22.2, 33.7, 45.9, 58.8, 69.0),
  Upper= c(21.0, 30.1, 42.4, 54.6, 66.7, 82.0)
)
bounds_japan = c(40,65)

# Median
median_lden = tapply(data_metrics$Lden, data_metrics$ID, median)
median_lden_HA = data.frame(
  Stat='Median',
  Lden=sort(median_lden),
  HA_WHO=regression_WHO(sort(median_lden)),
  HA_JAPAN=regression_japan(sort(median_lden)),
  HA_MO=regression_MO(sort(median_lden)),
  HA_ISO_MO=regression_ISO_Miedema(sort(median_lden))
)

# Energy average
energyavg_lden = tapply(data_metrics$Lden, data_metrics$ID, energyavg)
energyavg_lden_HA = data.frame(
  Stat='Energy Average',
  Lden=sort(energyavg_lden),
  HA_WHO=regression_WHO(sort(energyavg_lden)),
  HA_JAPAN=regression_japan(sort(energyavg_lden)),
  HA_MO=regression_MO(sort(energyavg_lden)),
  HA_ISO_MO=regression_ISO_Miedema(sort(energyavg_lden))
)
energyavg_lden_HA$Site = 'Monitoring site'
energyavg_lden_HA$Site = factor(energyavg_lden_HA$Site)

# NOTE: Time scale for ERFs is long-term, typically one year, so single-date maximum Ldens are not appropriate

# combo = rbind(median_lden_HA, energyavg_lden_HA)
# combo$Stat = factor(combo$Stat)
erf_names = c('Military (Yokoshima 2021)','Guideline (WHO 2018)', 'Standard (ISO 2016)')
erf_colors = c('royalblue', 'black', 'darkorchid2')
names(erf_colors) = erf_names
pt_size = 2.7
pt_alpha = 0.7

# Without points
p_ha = ggplot() +
  # Confidence intervals
  geom_ribbon(ci_iso_miedema, mapping=aes(x=Lden,ymin=Lower,ymax=Upper), fill='purple', alpha=0.1) +
  geom_ribbon(ci_japan, mapping=aes(x=Lden,ymin=Lower,ymax=Upper), fill='blue', alpha=0.1) +
  # Exposure-response functions
  # stat_function(fun=regression_WHO, xlim=c(bounds_who[1],100), color=erf_colors['Guideline (WHO 2018)'], linetype='dashed') +
  stat_function(fun=regression_WHO, xlim=bounds_who, linewidth=.7, aes(color='Guideline (WHO 2018)')) +
  # stat_function(fun=regression_ISO_Miedema, xlim=c(bounds_iso_miedema[1], 200), color=erf_colors['Standard (ISO 2016)'], linetype='dashed') +
  stat_function(fun=regression_ISO_Miedema, xlim=bounds_iso_miedema, linewidth=.7, aes(color='Standard (ISO 2016)')) +
  # stat_function(fun=regression_japan, xlim=c(bounds_japan[1],100), color=erf_colors['Military (Yokoshima 2021)'], linetype='dashed') +
  stat_function(fun=regression_japan, xlim=bounds_japan, linewidth=.7, aes(color='Military (Yokoshima 2021)')) +
  scale_color_manual(name='Exposure-response', values=erf_colors, breaks=erf_names) +
  # Plot configuration
  labs(title='Percent population estimated highly annoyed per site') +
  scale_x_continuous(name='Lden (dBA)', limits=c(45,76), oob=rescale_none) +
  scale_y_continuous(name='%HA', n.breaks=9, limits=c(0,90), oob=rescale_none)
print(p_ha)
ggsave(p_ha, file=paste0(ggsave_output_path, 'erf_ha.png'), width=ggsave_width, height=ggsave_height)

# With points
p_ha = ggplot() +
  # Confidence intervals
  geom_ribbon(ci_iso_miedema, mapping=aes(x=Lden,ymin=Lower,ymax=Upper), fill='purple', alpha=0.1) +
  geom_ribbon(ci_japan, mapping=aes(x=Lden,ymin=Lower,ymax=Upper), fill='blue', alpha=0.1) +
  # Exposure-response functions
  stat_function(fun=regression_WHO, xlim=c(bounds_who[1],100), color=erf_colors['Guideline (WHO 2018)'], linetype='dashed') +
  stat_function(fun=regression_WHO, xlim=bounds_who, size=.7, aes(color='Guideline (WHO 2018)')) +
  stat_function(fun=regression_ISO_Miedema, xlim=c(bounds_iso_miedema[1], 200), color=erf_colors['Standard (ISO 2016)'], linetype='dashed') +
  stat_function(fun=regression_ISO_Miedema, xlim=bounds_iso_miedema, size=.7, aes(color='Standard (ISO 2016)')) +
  stat_function(fun=regression_japan, xlim=c(bounds_japan[1],100), color=erf_colors['Military (Yokoshima 2021)'], linetype='dashed') +
  stat_function(fun=regression_japan, xlim=bounds_japan, size=.7, aes(color='Military (Yokoshima 2021)')) +
  scale_color_manual(name='Exposure-response', values=erf_colors, breaks=erf_names) +
  # Measurement points
  geom_point(data=energyavg_lden_HA, aes(x=Lden, y=HA_WHO, fill=Site), shape=21, size=pt_size, alpha=pt_alpha) +
  geom_point(data=energyavg_lden_HA, aes(x=Lden, y=HA_JAPAN, fill=Site), shape=21, size=pt_size, alpha=pt_alpha) +
  geom_point(data=energyavg_lden_HA, aes(x=Lden, y=HA_ISO_MO, fill=Site), shape=21, size=pt_size, alpha=pt_alpha) +
  scale_fill_manual(name='', values='white') +
  # Plot configuration
  labs(title='Percent population estimated highly annoyed per site') +
  scale_x_continuous(name='Lden (dBA)', limits=c(45,85), oob=rescale_none) +
  scale_y_continuous(name='%HA', n.breaks=9, limits=c(0,110), oob=rescale_none) +
  geom_hline(yintercept=100, linetype='dotted')
print(p_ha)
ggsave(p_ha, file=paste0(ggsave_output_path, 'erf_ha_points.png'), width=ggsave_width, height=ggsave_height)

# Table
ha_table = data.frame(sapply(rownames(energyavg_lden_HA), get_site_name_for_ID))
ha_table = cbind(ha_table, energyavg_lden_HA[,c('HA_JAPAN','HA_WHO','HA_ISO_MO','Lden')])
colnames(ha_table) = c('Site', 'Military', 'Guideline', 'Standard', 'Lden')
ha_table[,c('Military','Guideline','Standard')] = round(ha_table[,c('Military','Guideline','Standard')])
ha_table = ha_table[order(ha_table$Military, decreasing=T), ]
ha_table$Military[which(ha_table$Lden>bounds_japan[2])] = paste('≥', round(regression_japan(bounds_japan[2])))
ha_table$Guideline[which(ha_table$Lden>bounds_who[2])] = paste('≥', round(regression_WHO(bounds_who[2])))
ha_table$Standard[which(ha_table$Lden>bounds_iso_miedema[2])] = paste('≥', round(regression_ISO_Miedema(bounds_iso_miedema[2])))
p_ha_table = ggplot() +
  annotate(geom='table', size=4, x=0, y=0, label=list(ha_table[,!names(ha_table) %in% c('Lden')]), table.theme=ttheme_gtlight) + theme_void()
print(p_ha_table)
ggsave(p_ha_table, file=paste0(ggsave_output_path, 'erf_ha_table.png'), width=ggsave_width, height=ggsave_height)
