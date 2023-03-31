## Health and well-being impact assessment - Sleep disturbance

source('global.R')
source('data/metrics/metrics.R')

data_sites   = get_data_sites()
data_metrics = get_data_metrics()

# Sleep disturbance for nights of average and maximum Lnight, all sites ------
# Dependencies: any dataset
# What is the risk of high sleep disturbance at these sites based on exposure-response relationships?

# TODO: Military-specific / low-frequency / onset / aircraft dB penalty adjustment?

# Smith et al 2022 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9272916/
# Restricted only to survey questions where noise was explicitly mentioned
eq_hsd_awakenings = function(Lnight) {
  return(0.03132*(Lnight)^2 - 1.80203*(Lnight) + 31.28079)
}
eq_hsd_fallingasleep = function(Lnight) {
  return(0.02204*(Lnight)^2 - 0.86230*(Lnight) + 12.42449)
}
eq_hsd_sleepdisturbance = function(Lnight) {
  return(0.02664*(Lnight)^2 - 1.17389*(Lnight) + 16.46165)
}
eq_hsd_combinedestimate = function(Lnight) {
  return(0.025015*(Lnight)^2 - 1.12624*(Lnight) + 17.074213)
}

ci_upper_hsd_combinedestimate = function(Lnight) {
  return(0.0221483*Lnight^2 - 0.5720120*Lnight + 3.5979810)
}
ci_lower_hsd_combinedestimate = function(Lnight) {
  return(0.027883*Lnight^2 - 1.680477*Lnight + 30.550446) 
}
ci_hsd_combinedestimate = data.frame(
  Lnight = seq(from=40, to=65, by=1),
  Lower  = ci_lower_hsd_combinedestimate(seq(from=40, to=65, by=1)),
  Upper  = ci_upper_hsd_combinedestimate(seq(from=40, to=65, by=1))
)
bounds_hsd = c(40,65)

# NOTE: Limitations - "The rapid onset time in particular means that a given aircraft is probably more likely to induce an awakening than one that is much more gradual, like a civil aircraft. But of course physiological disturbance such as this and self-reported long-term %HSD are not the same thing, and do not necessarily correlate all that well."

# NOTE: Time scale for ERFs is long-term, typically one year, so single-date maximum Lnights are not appropriate. "Equivalent noise levels are often used in surveys and epidemiologic studies as long-term average exposure metrics, and are therefore also often found in legislative and policy contexts. For example, the Night Noise Guidelines for Europe of the World Health Organization (WHO) define effects of nocturnal noise based on annual average outdoor Lnight ranges. The value of equivalent noise levels in describing the effects of noise on sleep is more limited, as different noise scenarios may calculate to the same equivalent noise level, but differ substantially in their sleep disturbing properties. There is general agreement that the number and acoustical properties of single noise events better reflect the actual degree of nocturnal sleep disturbance in a single night. It is thus questionable whether Lnight can be used as the only indicator for predicting the effects of noise on sleep and the consequences of noise-induced sleep disturbance, or whether supplemental noise indicators are needed

# Median
median_lnight = tapply(data_metrics$Lden_Lnight, data_metrics$ID, median)
median_lnight_HSD = data.frame(
  Stat='Median',
  Lnight=sort(median_lnight),
  HSD_smith=eq_hsd_combinedestimate(sort(median_lnight))
)

# Energy average
energyavg_lnight = tapply(data_metrics$Lden_Lnight, data_metrics$ID, energyavg)
energyavg_lnight_HSD = data.frame(
  Stat='Energy Average',
  Lnight=sort(energyavg_lnight),
  HSD_smith=eq_hsd_combinedestimate(sort(energyavg_lnight))
)
energyavg_lnight_HSD$Site = 'Monitoring site'
energyavg_lnight_HSD$Site = factor(energyavg_lnight_HSD$Site)

# NOTE: Time scale for ERFs is long-term, typically one year, so single-date maximum Ldens are not appropriate
# combo = rbind(median_lnight_HSD, energyavg_lnight_HSD)

erf_names = c('Smith 2022')
erf_colors = c('darkorchid2')
names(erf_colors) = erf_names
pt_size = 2.7
pt_alpha = 0.7

# Without points
p_hsd = ggplot() +
  # Confidence intervals
  geom_ribbon(ci_hsd_combinedestimate, mapping=aes(x=Lnight,ymin=Lower,ymax=Upper), fill='purple', alpha=0.1) +
  # Exposure-response function(s)
  # stat_function(fun=eq_hsd_combinedestimate, xlim=c(bounds_hsd[2],80), linetype='dashed', color=erf_colors['Smith 2022']) +
  stat_function(fun=eq_hsd_combinedestimate, xlim=bounds_hsd, size=.7, aes(color='Smith 2022')) +
  scale_color_manual(name='Exposure-response', values=erf_colors, breaks=erf_names) +
  # Plot configuration
  scale_x_continuous(name='Lnight (dBA)', limits=c(40,65), oob=rescale_none) +
  scale_y_continuous(name='%HSD', n.breaks=9, limits=c(0,60), oob=rescale_none) +
  labs(title='Probability of high sleep disturbance per site')
print(p_hsd)
ggsave(p_hsd, file=paste0(ggsave_output_path, 'erf_hsd.png'), width=ggsave_width, height=ggsave_height)

# With points
p_hsd = ggplot() +
  # Confidence intervals
  geom_ribbon(ci_hsd_combinedestimate, mapping=aes(x=Lnight,ymin=Lower,ymax=Upper), fill='purple', alpha=0.1) +
  # Exposure-response function(s)
  stat_function(fun=eq_hsd_combinedestimate, xlim=c(bounds_hsd[2],80), linetype='dashed', color=erf_colors['Smith 2022']) +
  stat_function(fun=eq_hsd_combinedestimate, xlim=bounds_hsd, size=.7, aes(color='Smith 2022')) +
  scale_color_manual(name='Exposure-response', values=erf_colors, breaks=erf_names) +
  # Measurement points
  geom_point(data=energyavg_lnight_HSD, aes(x=Lnight, y=HSD_smith, fill=Site), shape=21, size=pt_size, alpha=pt_alpha) +
  scale_fill_manual(name='', values='white') +
  # Plot configuration
  scale_x_continuous(name='Lnight (dBA)', limits=c(40,80), oob=rescale_none) +
  scale_y_continuous(name='%HSD', n.breaks=9, limits=c(0,90), oob=rescale_none) +
  labs(title='Probability of high sleep disturbance per site')
print(p_hsd)
ggsave(p_hsd, file=paste0(ggsave_output_path, 'erf_hsd_points.png'), width=ggsave_width, height=ggsave_height)

# Table
hsd_table = data.frame(sapply(rownames(energyavg_lnight_HSD), get_site_name_for_ID))
hsd_table = cbind(hsd_table, energyavg_lnight_HSD[,c('HSD_smith','Lnight')])
hsd_table[,c('HSD_smith')] = round(hsd_table[,c('HSD_smith')])
colnames(hsd_table) = c('Site', '%HSD','Lnight')
hsd_table = hsd_table[order(hsd_table[,'%HSD'], decreasing=T), ]
hsd_table[which(hsd_table$Lnight>bounds_hsd[2]),'%HSD'] = paste('≥', round(eq_hsd_combinedestimate(bounds_hsd[2])))
p_hsd_table = ggplot() +
  annotate(geom='table', size=4, x=0, y=0, label=list(hsd_table[,!names(hsd_table) %in% c('Lnight')]), table.theme=ttheme_gtlight) + theme_void()
print(p_hsd_table)
ggsave(p_hsd_table, file=paste0(ggsave_output_path, 'erf_hsd_table.png'), width=ggsave_width, height=ggsave_height)