## Health and well-being impact assessment - Annoyance

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

## Annoyance -------------------------------------------------------------------
# What is the risk of high annoyance at these sites based on exposure-response relationships?
# Dependencies: any dataset
# See ISO 1996-1 2016 Annex E/F and Lct

###################################################################################################
# Modeled spatial exposure

r_Ldn = pop_exposure_stack[['Ldn']]
r_pop = pop_exposure_stack[['Impacted.Population']]
r_pop[r_pop == 0] = NA # set all 0 population cells to NA
r_Ldn[is.na(r_Ldn)] = 0 # set all NA exposure cells to 0

# Multiply population in each cell by %HA to yield estimate of # highly annoyed persons in that cell

percent_HA_WHO = calc(r_Ldn, fun=exp_resp_WHO_bounded)
estimated_pop_HA_WHO = percent_HA_WHO * 0.01 * r_pop
estimated_pop_HA_WHO[estimated_pop_HA_WHO == 0] = NA
# mapview(estimated_pop_HA_WHO)

percent_HA_ISO = calc(r_Ldn, fun=exp_resp_ISO_Miedema_bounded)
estimated_pop_HA_ISO = percent_HA_ISO * 0.01 * r_pop
estimated_pop_HA_ISO[estimated_pop_HA_ISO == 0] = NA
# mapview(estimated_pop_HA_ISO)

percent_HA_Yokoshima = calc(r_Ldn, fun=exp_resp_Yokoshima_bounded)
estimated_pop_HA_Yokoshima = percent_HA_Yokoshima * 0.01 * r_pop
estimated_pop_HA_Yokoshima[estimated_pop_HA_Yokoshima == 0] = NA
# mapview(estimated_pop_HA_Yokoshima)

# Number of people estimated to be highly annoyed
(npop_HA_WHO       = cellStats(estimated_pop_HA_WHO, 'sum')) # WHO
(npop_HA_ISO       = cellStats(estimated_pop_HA_ISO, 'sum')) # ISO
(npop_HA_Yokoshima = cellStats(estimated_pop_HA_Yokoshima, 'sum')) # Yokoshima

# Write layers to file
pop_HA_stack = stack(
  estimated_pop_HA_ISO,
  estimated_pop_HA_WHO,
  estimated_pop_HA_Yokoshima
)
mapview(pop_HA_stack)
names(pop_HA_stack) = c('HA_ISO', 'HA_WHO', 'HA_Yokoshima')
filename = glue('{output_path}/pop_HA_stack.grd')
writeRaster(brick(pop_HA_stack), filename = filename, overwrite = T)
message('Created ', filename)

###################################################################################################
# Measured site exposure

# Median
median_lden = tapply(data_metrics$Lden, data_metrics$ID, median)
median_lden_HA = data.frame(
  Stat='Median',
  Lden=sort(median_lden),
  HA_WHO=exp_resp_WHO(sort(median_lden)),
  HA_Yokoshima=exp_resp_Yokoshima(sort(median_lden)),
  HA_MO=exp_resp_MO(sort(median_lden)),
  HA_ISO_MO=exp_resp_ISO_Miedema(sort(median_lden))
)

# Energy average
energyavg_lden = tapply(data_metrics$Lden, data_metrics$ID, energyavg)
energyavg_lden_HA = data.frame(
  Stat='Energy Average',
  Lden=sort(energyavg_lden),
  HA_WHO=exp_resp_WHO(sort(energyavg_lden)),
  HA_Yokoshima=exp_resp_Yokoshima(sort(energyavg_lden)),
  HA_MO=exp_resp_MO(sort(energyavg_lden)),
  HA_ISO_MO=exp_resp_ISO_Miedema(sort(energyavg_lden))
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
  geom_ribbon(ci_Yokoshima, mapping=aes(x=Lden,ymin=Lower,ymax=Upper), fill='blue', alpha=0.1) +
  # Exposure-response functions
  # stat_function(fun=exp_resp_WHO, xlim=c(bounds_who[1],100), color=erf_colors['Guideline (WHO 2018)'], linetype='dashed') +
  stat_function(fun=exp_resp_WHO, xlim=bounds_who, linewidth=.7, aes(color='Guideline (WHO 2018)')) +
  # stat_function(fun=exp_resp_ISO_Miedema, xlim=c(bounds_iso_miedema[1], 200), color=erf_colors['Standard (ISO 2016)'], linetype='dashed') +
  stat_function(fun=exp_resp_ISO_Miedema, xlim=bounds_iso_miedema, linewidth=.7, aes(color='Standard (ISO 2016)')) +
  # stat_function(fun=exp_resp_Yokoshima, xlim=c(bounds_Yokoshima[1],100), color=erf_colors['Military (Yokoshima 2021)'], linetype='dashed') +
  stat_function(fun=exp_resp_Yokoshima, xlim=bounds_Yokoshima, linewidth=.7, aes(color='Military (Yokoshima 2021)')) +
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
  geom_ribbon(ci_Yokoshima, mapping=aes(x=Lden,ymin=Lower,ymax=Upper), fill='blue', alpha=0.1) +
  # Exposure-response functions
  stat_function(fun=exp_resp_WHO, xlim=c(bounds_who[1],100), color=erf_colors['Guideline (WHO 2018)'], linetype='dashed') +
  stat_function(fun=exp_resp_WHO, xlim=bounds_who, size=.7, aes(color='Guideline (WHO 2018)')) +
  stat_function(fun=exp_resp_ISO_Miedema, xlim=c(bounds_iso_miedema[1], 200), color=erf_colors['Standard (ISO 2016)'], linetype='dashed') +
  stat_function(fun=exp_resp_ISO_Miedema, xlim=bounds_iso_miedema, size=.7, aes(color='Standard (ISO 2016)')) +
  stat_function(fun=exp_resp_Yokoshima, xlim=c(bounds_Yokoshima[1],100), color=erf_colors['Military (Yokoshima 2021)'], linetype='dashed') +
  stat_function(fun=exp_resp_Yokoshima, xlim=bounds_Yokoshima, size=.7, aes(color='Military (Yokoshima 2021)')) +
  scale_color_manual(name='Exposure-response', values=erf_colors, breaks=erf_names) +
  # Measurement points
  geom_point(data=energyavg_lden_HA, aes(x=Lden, y=HA_WHO, fill=Site), shape=21, size=pt_size, alpha=pt_alpha) +
  geom_point(data=energyavg_lden_HA, aes(x=Lden, y=HA_Yokoshima, fill=Site), shape=21, size=pt_size, alpha=pt_alpha) +
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
ha_table = cbind(ha_table, energyavg_lden_HA[,c('HA_Yokoshima','HA_WHO','HA_ISO_MO','Lden')])
colnames(ha_table) = c('Site', 'Military', 'Guideline', 'Standard', 'Lden')
ha_table[,c('Military','Guideline','Standard')] = round(ha_table[,c('Military','Guideline','Standard')])
ha_table = ha_table[order(ha_table$Military, decreasing=T), ]
ha_table$Military[which(ha_table$Lden>bounds_Yokoshima[2])] = paste('≥', round(exp_resp_Yokoshima(bounds_Yokoshima[2])))
ha_table$Guideline[which(ha_table$Lden>bounds_who[2])] = paste('≥', round(exp_resp_WHO(bounds_who[2])))
ha_table$Standard[which(ha_table$Lden>bounds_iso_miedema[2])] = paste('≥', round(exp_resp_ISO_Miedema(bounds_iso_miedema[2])))
p_ha_table = ggplot() +
  annotate(geom='table', size=4, x=0, y=0, label=list(ha_table[,!names(ha_table) %in% c('Lden')]), table.theme=ttheme_gtlight) + theme_void()
print(p_ha_table)
ggsave(p_ha_table, file=paste0(ggsave_output_path, 'erf_ha_table.png'), width=ggsave_width, height=ggsave_height)
