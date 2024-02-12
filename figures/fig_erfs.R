## Figures for exposure-response functions

source('global.R')
source('metrics/exposure_response_functions.R')
library(patchwork)

source('figures/fig_global.R')

# Annoyance
erf_names = c('Military (Yokoshima 2021)','Guideline (WHO 2018)', 'Standard (ISO 2016)', 'Navy (FICON 1992)', 'National (FAA NES 2021)')
erf_colors = c('#e066ff', '#222222', '#F8766D', '#619CFF', '#00BA38')
names(erf_colors) = erf_names

p_HA = ggplot() +
  # Confidence intervals
  # geom_ribbon(pi_iso_miedema, mapping=aes(x=dB5,ymin=Lower,ymax=Upper), fill='#F8766D', alpha=0.1) +
  geom_ribbon(ci_Yokoshima, mapping=aes(x=Lden,ymin=Lower,ymax=Upper), fill='#e066ff') +
  geom_ribbon(ci_FAANES, mapping=aes(x=Ldn,ymin=Lower,ymax=Upper), fill='#00BA38') +
  # Exposure-response functions
  stat_function(fun=exp_resp_WHO, xlim=bounds_who, linewidth=.7, aes(color='Guideline (WHO 2018)')) +
  stat_function(fun=exp_resp_ISO_Miedema, xlim=bounds_iso_miedema, linewidth=.7, aes(color='Standard (ISO 2016)')) +
  stat_function(fun=exp_resp_Yokoshima, xlim=bounds_Yokoshima, linewidth=.7, aes(color='Military (Yokoshima 2021)')) +
  stat_function(fun=exp_resp_FICON, xlim=bounds_FICON, linewidth=.7, aes(color='Navy (FICON 1992)')) +
  stat_function(fun=exp_resp_FAANES, xlim=bounds_FAANES, linewidth=.7, aes(color='National (FAA NES 2021)')) +
  scale_color_manual(name='Exposure-response', values=erf_colors, breaks=erf_names) +
  # Plot configuration
  labs(title='High annoyance') +
  scale_x_continuous(name=expression(L[dn]~'dB('*A*')'), limits=c(40,90), oob=rescale_none) +
  scale_y_continuous(name='%HA', n.breaks=9, limits=c(0,90), oob=rescale_none)
print(p_HA)
ggsave(p_HA, file=paste0(output_path, 'erf_HA.png'), width=ggsave_width, height=ggsave_height)

# Sleep disturbance
erf_names = c('Guideline (WHO 2018)', 'Updated WHO Guideline (Smith 2022)')
erf_colors = c('black', '#20dfdf')
names(erf_colors) = erf_names

p_HSD = ggplot() +
  # Confidence intervals
  geom_ribbon(ci_HSD_combinedestimate, mapping=aes(x=Lnight,ymin=Lower,ymax=Upper), fill='#20dfdf') +
  geom_ribbon(ci_HSD_WHO, mapping=aes(x=Lnight,ymin=Lower,ymax=Upper), fill='gray') +
  # Exposure-response function(s)
  # stat_function(fun=exp_resp_HSD_combinedestimate, xlim=c(bounds_HSD[2],80), linetype='dashed', color=erf_colors['Smith 2022']) +
  stat_function(fun=exp_resp_HSD_WHO, xlim=bounds_HSD, linewidth=.7, aes(color='Guideline (WHO 2018)')) +
  stat_function(fun=exp_resp_HSD_Smith, xlim=bounds_HSD, linewidth=.7, aes(color='Updated WHO Guideline (Smith 2022)')) +
  scale_color_manual(name='Exposure-response', values=erf_colors, breaks=erf_names) +
  # Plot configuration
  labs(title='High sleep disturbance') +
  scale_x_continuous(name=expression(L[night]~'dB('*A*')'), limits=c(40,65), oob=rescale_none) +
  scale_y_continuous(name='%HSD', n.breaks=9, limits=c(0,60), oob=rescale_none)
print(p_HSD)
  ggsave(p_HSD, file=paste0(output_path, 'erf_HSD.png'), width=ggsave_width, height=ggsave_height)

p_theme = theme(
  plot.title=element_text(size=text_size_max),
  text=element_text(size=text_size_max),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position='none'
)  

p_combined = 
  p_HA + p_theme +
  p_HSD + p_theme +
  plot_annotation(tag_levels = 'A'); p_combined

ggsave(p_combined, file=paste0(output_path, 'erfs.png'), width=10, height=5.4)
ggsave(
  plot = cowplot::get_legend(p_HA),
  filename = glue('{output_path}/legend_HA.png')
)
ggsave(
  plot = cowplot::get_legend(p_HSD),
  filename = glue('{output_path}/legend_HSD.png')
)

ggplot2::ggsave(filename = glue('{output_path}/erfs.eps'),
                device = 'eps', units = 'cm', dpi = 300, 
                width = fig_size_double - 3.5, height = fig_size_single - 1.0,
                plot = p_combined + theme(
                  legend.position = 'none'
                ))
