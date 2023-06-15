## Health and well-being impact assessment - Sleep disturbance

source('global.R')
source('metrics/metrics.R')
source('metrics/exposure_response_functions.R')
source('analysis/population_exposure.R')

data_sites   = get_data_sites()
data_metrics = get_data_metrics()

# Sleep disturbance for nights of average and maximum Lnight, all sites ---------------------------
# Dependencies: any dataset
# What is the risk of high sleep disturbance at these sites based on exposure-response relationships?

###################################################################################################
# Modeled spatial exposure

# Multiply pop in each exposure intersection by %HSD to yield estimate of # highly sleep-disturbed persons
exposure_Lnight$pop_HSD = round(sapply(as.numeric(exposure_Lnight$Level), exp_resp_HSD_combinedestimate_bounded) * 0.01 * exposure_Lnight$subpopulation)

mapview(exposure_Lnight, zcol='subpopulation', layer.name='Population') +
  mapview(exposure_Lnight, zcol='pop_HSD', at=seq(0, 350, 50), layer.name='Population HSD')

# WHO - "For night noise exposure, the GDG strongly recommends reducing noise levels produced by aircraft during night time below 40 dB Lnight, as night-time aircraft noise above this level is associated with adverse effects on sleep."
lnight_impact_threshold = 40
mapview(exposure_Lnight[as.numeric(exposure_Lnight$Level)>=lnight_impact_threshold,], zcol='Level') + mapview(population_bg, col.regions=list('white'))

# Estimated number of people subject to nighttime noise exposure levels associated with adverse sleep effects
sum(st_drop_geometry(exposure_Lnight[exposure_Lnight$Level>=lnight_impact_threshold, ])$subpopulation)

# Number of people estimated to be highly sleep disturbed according to WHO (Smith expanded) guidelines...
sum(st_drop_geometry(exposure_Lnight)$pop_HSD)

###################################################################################################
# Measured site exposure

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
