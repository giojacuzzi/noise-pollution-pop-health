## Health and well-being impact assessment - Sleep disturbance

source('global.R')
source('metrics/metrics.R')
source('metrics/exposure_response_functions.R')

data_sites   = get_data_sites()
data_metrics = get_data_metrics()

library(raster)
library(glue)
library(mapview)
mapviewOptions(mapview.maxpixels = 50000000)
input_path = paste0(here::here(), '/analysis/_output')
output_path = paste0(here::here(), '/analysis/_output')
pop_exposure_stack = stack(glue('{input_path}/pop_exposure_stack.grd'))

# Sleep disturbance ---------------------------
# Dependencies: any dataset
# What is the risk of high sleep disturbance based on exposure-response relationships?

###################################################################################################
# Measured site exposure (nights of average and maximum Lnight, all sites)

# Median
median_lnight = tapply(data_metrics$Lden_Lnight, data_metrics$ID, median)
median_lnight_HSD = data.frame(
  Stat='Median',
  Lnight=sort(median_lnight),
  HSD_smith=exp_resp_HSD_combinedestimate(sort(median_lnight))
)

# Energy average
energyavg_lnight = tapply(data_metrics$Lden_Lnight, data_metrics$ID, energyavg)
energyavg_lnight_HSD = data.frame(
  Stat='Energy Average',
  Lnight=sort(energyavg_lnight),
  HSD_smith=exp_resp_HSD_combinedestimate(sort(energyavg_lnight))
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
p_HSD = ggplot() +
  # Confidence intervals
  geom_ribbon(ci_HSD_combinedestimate, mapping=aes(x=Lnight,ymin=Lower,ymax=Upper), fill='purple', alpha=0.1) +
  # Exposure-response function(s)
  # stat_function(fun=exp_resp_HSD_combinedestimate, xlim=c(bounds_HSD[2],80), linetype='dashed', color=erf_colors['Smith 2022']) +
  stat_function(fun=exp_resp_HSD_combinedestimate, xlim=bounds_HSD, linewidth=.7, aes(color='Smith 2022')) +
  scale_color_manual(name='Exposure-response', values=erf_colors, breaks=erf_names) +
  # Plot configuration
  scale_x_continuous(name='Lnight (dBA)', limits=c(40,65), oob=rescale_none) +
  scale_y_continuous(name='%HSD', n.breaks=9, limits=c(0,60), oob=rescale_none) +
  labs(title='Probability of high sleep disturbance per site')
print(p_HSD)
ggsave(p_HSD, file=paste0(ggsave_output_path, 'erf_HSD.png'), width=ggsave_width, height=ggsave_height)

# With points
p_HSD = ggplot() +
  # Confidence intervals
  geom_ribbon(ci_HSD_combinedestimate, mapping=aes(x=Lnight,ymin=Lower,ymax=Upper), fill='purple', alpha=0.1) +
  # Exposure-response function(s)
  stat_function(fun=exp_resp_HSD_combinedestimate, xlim=c(bounds_HSD[2],80), linetype='dashed', color=erf_colors['Smith 2022']) +
  stat_function(fun=exp_resp_HSD_combinedestimate, xlim=bounds_HSD, linewidth=.7, aes(color='Smith 2022')) +
  scale_color_manual(name='Exposure-response', values=erf_colors, breaks=erf_names) +
  # Measurement points
  geom_point(data=energyavg_lnight_HSD, aes(x=Lnight, y=HSD_smith, fill=Site), shape=21, size=pt_size, alpha=pt_alpha) +
  scale_fill_manual(name='', values='white') +
  # Plot configuration
  scale_x_continuous(name='Lnight (dBA)', limits=c(40,80), oob=rescale_none) +
  scale_y_continuous(name='%HSD', n.breaks=9, limits=c(0,90), oob=rescale_none) +
  labs(title='Probability of high sleep disturbance per site')
print(p_HSD)
ggsave(p_HSD, file=paste0(ggsave_output_path, 'erf_HSD_points.png'), width=ggsave_width, height=ggsave_height)

# Table
hsd_table = data.frame(sapply(rownames(energyavg_lnight_HSD), get_site_name_for_ID))
hsd_table = cbind(hsd_table, energyavg_lnight_HSD[,c('HSD_smith','Lnight')])
hsd_table[,c('HSD_smith')] = round(hsd_table[,c('HSD_smith')])
colnames(hsd_table) = c('Site', '%HSD','Lnight')
hsd_table = hsd_table[order(hsd_table[,'%HSD'], decreasing=T), ]
hsd_table[which(hsd_table$Lnight>bounds_HSD[2]),'%HSD'] = paste('≥', round(exp_resp_HSD_combinedestimate(bounds_HSD[2])))
p_HSD_table = ggplot() +
  annotate(geom='table', size=4, x=0, y=0, label=list(hsd_table[,!names(hsd_table) %in% c('Lnight')]), table.theme=ttheme_gtlight) + theme_void()
print(p_HSD_table)
ggsave(p_HSD_table, file=paste0(ggsave_output_path, 'erf_HSD_table.png'), width=ggsave_width, height=ggsave_height)
