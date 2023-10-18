library(tidyr)

source('global.R')
source('figures/fig_global.R')

# From Navy report to Congress:
# - "For these summaries, one flight operation is counted whenever an aircraft touches or leaves a runway surface. Thus, an arrival and a departure each count as one flight operation, whereas a closed pattern, such as an FCLP, counts as two flight operations for each circuit."
# - "The number of operations used by the NOISEMAP model is based on the average annual day, per DoD Instruction 4715.13. The average annual day represents the average number of daily airfield operations that would occur during a 24-hour period based on 365 flying days per year; the average annual day is calculated by dividing the total annual airfield operations by 365."

library(readr)
mp4_ops = read_csv("data/simulation/_output/csv/NASWI_Noisemap - Flight Operations Combined Average.csv")

closed_pattern_ops = 2 * sum(mp4_ops[mp4_ops$`Track Type` == 'Closed Pattern', 'Num Total'])
other_ops = sum(mp4_ops[mp4_ops$`Track Type` != 'Closed Pattern', 'Num Total'])

nops_4MP = (closed_pattern_ops + other_ops) * 365
msg('Total projected annual operations from 2020-2021 four monitoring periods:', nops_4MP)

nops_A2A = 112100 # 1.200704x, Alternative 2A
# nops_NA  = 84700 # 0.9072226x, No Action

results_exposure = read.csv('analysis/population_noise_exposure/_output/population_noise_exposure_summary.csv') # 2021 4-month monitoring period aggregate ops
results_exposure = results_exposure[nrow(results_exposure),]
results_exposure$Exposed = as.numeric(results_exposure$Exposed)
results_exposure$Operations = nops_4MP
results_exposure$Multiplier = 1.0

results_impacts = read.csv('analysis/population_health_impacts/_output/health_impact_summary.csv') # 2021 4-month monitoring period aggregate ops
results_impacts = results_impacts[nrow(results_impacts),]

results = merge(results_impacts, results_exposure)
results = results[, c('Multiplier', 'Operations', 'Exposed', 'HA_WHO', 'HSD_WHO')]

multipliers = unique(sapply(str_split(list.files('analysis/_output/alternatives'), '_'), '[[', 1))

for (m in multipliers) {
  file_exposure = paste0('analysis/_output/alternatives/', m, '_population_noise_exposure_summary.csv', sep = '')
  file_impacts = paste0('analysis/_output/alternatives/', m, '_health_impact_summary.csv', sep = '')
  
  m = as.numeric(gsub('x', '', m))
  results_exposure = read.csv(file_exposure)
  results_exposure = results_exposure[nrow(results_exposure),]
  results_exposure$Exposed = as.numeric(results_exposure$Exposed)
  results_exposure$Operations = nops_4MP * m
  results_exposure$Multiplier = m
  
  results_impacts = read.csv(file_impacts)
  results_impacts = results_impacts[nrow(results_impacts),]
  
  results_temp = merge(results_impacts, results_exposure)
  results_temp = results_temp[, c('Multiplier', 'Operations', 'Exposed', 'HA_WHO', 'HSD_WHO')]
  results = rbind(results, results_temp)
}

results = results[order(results$Operations,decreasing=T),]
results = results %>% mutate_if(is.character, as.numeric)
results = results[, c('Operations', 'Exposed', 'HA_WHO', 'HSD_WHO')]

results_long = round(results / 1000.0, 2) %>% pivot_longer(cols=names(results)[2:ncol(results)], names_to='Outcome', values_to='Estimate')

annotation_size = text_size_min/.pt

p_simulations = results_long %>% ggplot(aes(x=Operations, y=Estimate, group=Outcome, color=Outcome)) +
  geom_vline(aes(xintercept=nops_4MP / 1000.0), linetype = 'dashed', color = 'darkgray') +
  geom_vline(aes(xintercept=nops_A2A / 1000.0), linetype = 'dashed', color = 'darkgray') +
  annotate('text', x=(nops_4MP - 3000) / 1000.0, y=50, label='This study', angle=90, color = 'darkgray', size = annotation_size) +
  annotate('text', x=(nops_A2A - 3000) / 1000.0, y=50, label='Projected 2021 total', angle=90, color = 'darkgray', size = annotation_size) +
  geom_line() +
  # geom_point() +
  annotate('text', x=65, y=70, label='Exposed population', angle=16, color = '#444444', size = annotation_size) +
  annotate('text', x=70, y=22, label='Highly annoyed (WHO)', angle=7, color = '#F8766D', size = annotation_size) +
  annotate('text', x=70, y=10, label='Highly sleep disturbed (WHO)', angle=3, color = '#619CFF', size = annotation_size) +
  scale_color_manual(labels=c('Exposed population','Highly annoyed (WHO)','Highly sleep disturbed (WHO)'), values=c('#444444','#F8766D', '#619CFF')) +
  scale_x_continuous(sec.axis = ggplot2::sec_axis(~. / nops_4MP * 1000.0, name = 'Scaling factor', labels = scales::label_percent())) +
  labs(title = '', x = 'Annual airfield operations (thousands)', y = 'Population impacted (thousands)', color = 'Health outcome') + 
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    text=element_text(size=text_size_max),
    legend.position = 'none'
  ); print(p_simulations)
ggsave(p_simulations, file=paste0('figures/_output/simulations.png'), width=10, height=10)

ggplot2::ggsave(filename = glue('{output_path}/simulations.eps'), 
                device = 'eps', units = 'cm', dpi = 300, 
                width = fig_size_single - 0.2, height = fig_size_single + 0.4,
                plot = p_simulations + theme(
                  panel.grid.minor.x = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  legend.position = 'none'))
