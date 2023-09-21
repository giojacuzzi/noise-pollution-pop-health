# Various figures
library(OpenStreetMap)
library(osmdata)

source('figures/fig_global.R')
source('simulation/contours.R')

source('metrics/metrics.R')
source('metrics/thresholds.R')
source('metrics/exposure_response_functions.R')

input_path = paste0(here::here(), '/analysis/population_noise_exposure/_output/noise_exposure')
pop_exposure_stack = stack(glue('{input_path}/pop_exposure_stack.grd'))

# Level scale to unify colors across plots
level_scale = seq(40,90,5)

# Population exposure per 5 dB
exposure_levels_Ldn = data.frame()
for (dB in unique(na.omit(as.vector(pop_exposure_stack[['Ldn']])))) {
  message('Exposure ', dB, ' dB')
  exp_masked = mask(pop_exposure_stack[['Exposed.Population']],
                    clamp(pop_exposure_stack[['Ldn']], lower=dB, upper=dB, useValues=F))
  exposure_levels_Ldn = rbind(exposure_levels_Ldn, data.frame(
    Level=dB,
    Population=cellStats(exp_masked, 'sum')
  ))
}
exposure_levels_Ldn = exposure_levels_Ldn %>%
  mutate(Level = (Level) %/% 5 * 5) %>%
  group_by(Level) %>%
  summarise(Population = sum(Population))

exposure_levels_Ldn$Level = factor(exposure_levels_Ldn$Level, levels=level_scale)
exposure_levels_Ldn = exposure_levels_Ldn[exposure_levels_Ldn$Population != 0, ]
exposure_levels_Ldn$PopulationThousands = round(exposure_levels_Ldn$Population/1000.0,2)
p_pop_exposed_per_5dB = ggplot(exposure_levels_Ldn, aes(x=Level, y=PopulationThousands, fill=Level)) +
  geom_bar(position='dodge', stat='identity') +
  scale_fill_viridis_d(option='plasma', drop = F, name=expression(L[dn]~'dB('*A*')')) +
  scale_y_continuous(limits = c(0,25), expand = c(0, 0.5)) +
  ggtitle(expression('Estimated population exposed per 5 dB'~L[dn])) +
  xlab(expression(L[dn]~'dB('*A*')')) +
  ylab('Population (thousands)') +
  geom_text(aes(label=PopulationThousands), position=position_dodge(width=0.9), vjust=-0.25, size=7, color='#555555') +
  theme_minimal() + theme(
    legend.position = 'none',
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.length.x = unit(-0.1, 'pt'),
    axis.text.y = element_blank(),
    axis.title.x = element_text(margin = unit(c(4, 0, 0, 0), 'mm')),
    axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), 'mm')),
    axis.ticks = element_blank()
  ); p_pop_exposed_per_5dB
ggsave(p_pop_exposed_per_5dB + theme(text=element_text(size=23), plot.margin = margin(1,1,1,1, 'cm')), file=glue('{output_path}/pop_exposed_per_5dB.png'), width=10, height=9)

# Plot maps

bounds_x = c(-123.0, -122.32) # [min, max]
bounds_y = c(48.05, 48.55)
bounds = data.frame(
  x = c(bounds_x[1], bounds_x[1], bounds_x[2], bounds_x[2]),
  y = c(bounds_y[1], bounds_y[2], bounds_y[2], bounds_y[1])
)

contours = list(
  get_contours_Ldn(), #(threshold = lden_impact_threshold),
  get_contours_Lnight(), #(threshold = lnight_impact_threshold),
  get_contours_Leq24())#(threshold = HL_leq24_impact_threshold))

# contours[[1]]$Level = factor(contours[[1]]$Level, levels=level_scale)
# contours[[2]]$Level = factor(contours[[2]]$Level, levels=level_scale)
# contours[[3]]$Level = factor(contours[[3]]$Level, levels=level_scale)

# Combine contours in 5 dB increments
contours_5dB = contours
for (i in 1:3) {
  c = st_make_valid(contours[[i]])
  c_5dB = NULL
  for (l in seq(from=min(c$Level), to=max(c$Level), by=5)) {
    message(l)
    u = st_union(c[c$Level >= l & c$Level < l + 5, ])
    u = st_make_valid(st_as_sf(u))
    u$Level = l
    if (is.null(c_5dB)) {
      c_5dB = u
    }
    c_5dB = c_5dB %>% bind_rows(u)
  }
  contours_5dB[[i]] = c_5dB
}

cols = data.frame( # shared color scale among all level metrics
  level=level_scale,
  color=viridis_pal(option='C')(length(level_scale))
)

plot_contours = function(contours, threshold, lims, title, units, lwd) {
  below = contours[as.numeric(contours$Level)<(threshold) & as.numeric(contours$Level)>=(threshold-10),]
  contours = contours[as.numeric(contours$Level)>=threshold,]
  contours$Level = factor(contours$Level, levels=level_scale)
  
  p = ggplot() +
    geom_sf(data = naswi_land, fill = 'white') +
    # geom_sf(data = native_areas$geometry, fill='white', color='#FFFFFF00') +
    geom_sf(data = wa_roads$geometry, color='#DDDDDD', lwd=0.25) +
    geom_sf(data = native_areas$geometry, fill='#FFFFFF00', linetype='dotted') +
    geom_sf(data = wa_military$geometry, fill=color_military, color='#444444', lwd=0.4) +
    geom_sf(data = naswi_land, fill=NA) +
    geom_sf(data = runways, lwd=1, color='#555555') +
  
    geom_sf(data = st_cast(below[1,], 'MULTILINESTRING'), color='black', lwd=lwd, alpha=0.5, linetype='dashed') +

    geom_sf(data = contours, aes(fill=Level, color=Level), lwd=lwd) +
    scale_fill_manual(values=alpha(cols[cols$level %in% lims, 'color'], 0.6), name = units, guide = guide_legend(reverse = T)) +

    scale_color_manual(values=alpha(cols[cols$level %in% lims, 'color'], 0.8), name = units, guide = guide_legend(reverse = T)) +

    coord_sf(xlim = bounds_x, ylim = bounds_y, expand = F) +
    labs(caption = title, x='', y='') +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.key.size = unit(50, 'pt'),
          legend.text = element_text(size=20),
          legend.title = element_text(size=22),
          plot.caption = element_text(hjust = 0.5, size = 24),
          panel.background=element_rect(fill = '#EAF7FE', color = NA),
          panel.border=element_rect(colour = '#222222', fill=NA, linewidth=0.75),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          plot.margin = margin(0, 0, 0, 0, 'pt'))
  return(p)
}
p_Ldn = plot_contours(contours_5dB[[1]],
                     threshold = threshold_adverse_health_effects_Lden, lims = seq(45,90,5),
                     title = expression(L[dn]~'(45+ dB)'),
                     units = expression('dB('*A*')'), lwd=0.3) +
        geom_text(data = labels_large, aes(x = Lon, y = Lat, label = Name), size = 3.0, col = '#444444') +
        geom_text(data = labels_medium, aes(x = Lon, y = Lat, label = Name), size = 2.5, col = '#444444') +
        geom_text(data = labels_small, aes(x = Lon, y = Lat, label = Name), size = 2.25, col = '#444444'); p_Ldn
p_Lnight = plot_contours(contours_5dB[[2]],
                         threshold = threshold_sleep_disturbance_Lnight, lims = seq(40,90,5),
                         title = expression(L[night]~'(40+ dB)'),
                         units = expression('dB('*A*')'), lwd=0.2); p_Lnight
p_Leq24 = plot_contours(contours_5dB[[3]],
                        threshold = threshold_hearing_impairment_Leq24, lims = seq(70,90,5),
                        title = expression(L[eq24]~'(70+ dB)'),
                        units = expression('dB('*A*')'), lwd=0.2); p_Leq24
ggsave(p_Ldn + theme(text=element_text(size=22), plot.margin = margin(5,5,5,5, 'pt')), file=glue('{output_path}/contours_Ldn.png'), width=12, height=10)
ggsave(p_Lnight + theme(text=element_text(size=22), plot.margin = margin(5,5,5,5, 'pt')), file=glue('{output_path}/contours_Lnight.png'), width=12, height=10)
ggsave(p_Leq24 + theme(text=element_text(size=22), plot.margin = margin(5,5,5,5, 'pt')), file=glue('{output_path}/contours_Leq24.png'), width=12, height=10)

p_theme = theme(
  text=element_text(size=24),
  # axis.line=element_blank(),
  # axis.text.x=element_blank(),
  # axis.text.y=element_blank(),
  # axis.ticks=element_blank(),
  # axis.title.x=element_blank(),
  # axis.title.y=element_blank(),
  legend.position='none'
  # panel.background=element_rect(fill = '#EAF7FE', color = NA),
  # panel.border=element_rect(colour = '#222222', fill=NA, linewidth=0.75),
  # panel.grid.major=element_blank(),
  # panel.grid.minor=element_blank(),
  # plot.background=element_blank(),
  # plot.margin = margin(0, 0, 0, 0, 'pt')
)

p_combined = ((p_Ldn + p_theme) + ((p_Lnight + p_theme) / (p_Leq24 + p_theme))) + plot_annotation(tag_levels = 'A')

ggsave(
  plot = p_combined,
  filename = glue('{output_path}/contours.png'),
  width = 20, height = 20
)

ggsave(
  plot = cowplot::get_legend(p_Ldn),
  filename = glue('{output_path}/legend_Ldn.png')
)
ggsave(
  plot = cowplot::get_legend(p_Lnight),
  filename = glue('{output_path}/legend_Lnight.png')
)
