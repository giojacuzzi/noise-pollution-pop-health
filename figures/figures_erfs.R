source('global.R')
source('metrics/exposure_response_functions.R')

# Annoyance
erf_names = c('Military (Yokoshima 2021)','Guideline (WHO 2018)', 'Standard (ISO 2016)', 'Navy (FICON 1992)*', 'National (FAA NES 2021)*')
erf_colors = c('red', 'black', 'darkorchid2', 'royalblue', 'forestgreen')
names(erf_colors) = erf_names
pt_size = 2.7
pt_alpha = 0.7

p_ha = ggplot() +
  # Confidence intervals
  geom_ribbon(pi_iso_miedema, mapping=aes(x=dB5,ymin=Lower,ymax=Upper), fill='purple', alpha=0.1) +
  geom_ribbon(ci_Yokoshima, mapping=aes(x=Lden,ymin=Lower,ymax=Upper), fill='red', alpha=0.1) +
  geom_ribbon(ci_FAANES, mapping=aes(x=Ldn,ymin=Lower,ymax=Upper), fill='forestgreen', alpha=0.1) +
  # Exposure-response functions
  stat_function(fun=exp_resp_WHO, xlim=bounds_who, linewidth=.7, aes(color='Guideline (WHO 2018)')) +
  stat_function(fun=exp_resp_ISO_Miedema, xlim=bounds_iso_miedema, linewidth=.7, aes(color='Standard (ISO 2016)')) +
  stat_function(fun=exp_resp_Yokoshima, xlim=bounds_Yokoshima, linewidth=.7, aes(color='Military (Yokoshima 2021)')) +
  stat_function(fun=exp_resp_FICON, xlim=bounds_FICON, linewidth=.7, aes(color='Navy (FICON 1992)*')) +
  stat_function(fun=exp_resp_FAANES, xlim=bounds_FAANES, linewidth=.7, aes(color='National (FAA NES 2021)*')) +
  scale_color_manual(name='Exposure-response', values=erf_colors, breaks=erf_names) +
  # Plot configuration
  labs(title='Probability of high annoyance in response to aircraft noise') +
  scale_x_continuous(name=expression(L[den]~'dB('*A*')'), limits=c(40,90), oob=rescale_none) +
  scale_y_continuous(name='%HA', n.breaks=9, limits=c(0,90), oob=rescale_none)
print(p_ha)
ggsave(p_ha, file=paste0(ggsave_output_path, 'erf_ha.png'), width=ggsave_width, height=ggsave_height)

# Sleep disturbance
erf_names = c('WHO', 'Updated WHO Guideline (Smith 2022)')
erf_colors = c('black', 'orange')
names(erf_colors) = erf_names
pt_size = 2.7
pt_alpha = 0.7

p_HSD = ggplot() +
  # Confidence intervals
# geom_ribbon(ci_HSD_combinedestimate, mapping=aes(x=Lnight,ymin=Lower,ymax=Upper), fill='purple', alpha=0.1) +
  # Exposure-response function(s)
  # stat_function(fun=exp_resp_HSD_combinedestimate, xlim=c(bounds_HSD[2],80), linetype='dashed', color=erf_colors['Smith 2022']) +
  stat_function(fun=exp_resp_HSD_WHO, xlim=bounds_HSD, linewidth=.7, aes(color='WHO')) +
  stat_function(fun=exp_resp_HSD_Smith, xlim=bounds_HSD, linewidth=.7, aes(color='Updated WHO Guideline (Smith 2022)')) +
  scale_color_manual(name='Exposure-response', values=erf_colors, breaks=erf_names) +
  # Plot configuration
  scale_x_continuous(name=expression(L[night]~'dB('*A*')'), limits=c(40,65), oob=rescale_none) +
  scale_y_continuous(name='%HSD', n.breaks=9, limits=c(0,60), oob=rescale_none) +
  labs(title='Probability of high sleep disturbance in response to aircraft noise')
print(p_HSD)
ggsave(p_HSD, file=paste0(ggsave_output_path, 'erf_HSD.png'), width=ggsave_width, height=ggsave_height)
