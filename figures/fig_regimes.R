library(tidyr)

source('global.R')

results = read.csv('analysis/_output/health_impact_summary.csv') # 2021 4-month monitoring period aggregate ops
nops_4MP = 93361.84
nops_A2A = 112100 # 1.200704x, Alternative 2A
nops_NA  = 84700 # 0.9072226x, No Action

results = results[nrow(results),]
results$Exposed = as.numeric(results$Exposed)
results$Operations = nops_4MP
results$Multiplier = 1.0

summary_files = list.files('analysis/_output/alternatives', pattern = paste0('health_impact_summary*.csv'), full.names = T)
for (file in summary_files) {
  result = read.csv(file)
  multiplier = as.numeric(unlist(str_split(unlist(str_split(file, 'analysis/_output/alternatives/'))[2], 'x'))[1])
  nops = nops_4MP * multiplier
  msg(multiplier, nops)
  
  result = result[nrow(result),]
  result$Exposed = as.numeric(result$Exposed)
  result$Operations = nops
  result$Multiplier = multiplier
  
  results = rbind(results, result)
}

results = results[order(results$Operations,decreasing=T),]

# 
# # sim_A2A = read.csv('analysis/_output/alternatives/health_impact_summary_Alternative2A.csv') # 2018 EIS Alternative 2A annual ops (2021 projection)
# # sim_NA = read.csv('analysis/_output/alternatives/health_impact_summary_PreExpansion.csv') # 2018 EIS No Action
# 
# 
# # 140,042.8 (50 % increase) *
# # 130,000
# # 120,000
# # 112,100 ---- A2A
# # 110,000
# # 100,000
# # 93,361.84 ---- 4MP
# # 90,000
# # 84,700 ---- No Action
# # 80,000
# # 46,680.92 (50 % decrease) *
# 
# last_row = nrow(sim_4MP)
# comp = rbind(
#   sim_4MP[last_row,],
#   sim_A2A[last_row,],
#   sim_NA[last_row,]
# )

# nstats = ncol(results) - 1

# outcome_colors = c('yellow', 'royalblue', 'darkorchid2', 'black', 'forestgreen', 'red')
# names(outcome_colors) = names(comp)[3:nstats]

names(results) = c('Name', 'Population', 'Exposed Population', 'Highly Annoyed (FICON)', 'Highly Annoyed (ISO)', 'Highly Annoyed (WHO)', 'Highly Annoyed (FAA NES)', 'Highly Annoyed (Yokoshima)', 'Highly Sleep Disturbed (WHO)', 'Highly Sleep Disturbed (Smith)', 'Hearing Loss and Cardiovascular Effects', 'Operations', 'Multiplier')

results = results[, c('Operations', 'Highly Annoyed (WHO)', 'Highly Sleep Disturbed (WHO)', 'Exposed Population')]

results_long = round(results / 1000.0, 2) %>% pivot_longer(cols=names(results)[2:ncol(results)], names_to='Outcome', values_to='Estimate')

p_simulations = results_long %>% ggplot(aes(x=Operations, y=Estimate, group=Outcome, color=Outcome)) +
  geom_vline(aes(xintercept=nops_4MP / 1000.0), linetype = 'dashed', color = 'gray') +
  geom_vline(aes(xintercept=nops_A2A / 1000.0), linetype = 'dashed', color = 'gray') +
  annotate('text', x=(nops_4MP - 3000) / 1000.0, y=50, label='This study', angle=90, color = 'gray') +
  annotate('text', x=(nops_A2A - 3000) / 1000.0, y=50, label='Projected 2021 total', angle=90, color = 'gray') +
  geom_line() +
  geom_point() +
  annotate('text', x=65, y=66.5, label='Exposed population', angle=17, color = '#444444', size = 20) +
  annotate('text', x=65, y=20, label='Highly annoyed (WHO)', angle=6, color = '#F8766D', size = 20) +
  annotate('text', x=67, y=9, label='Highly sleep disturbed (WHO)', angle=2, color = '#619CFF', size = 20) +
  scale_color_manual(labels=c('Exposed population','Highly annoyed (WHO)','Highly sleep disturbed (WHO)'), values=c('#444444','#F8766D', '#619CFF')) +
  scale_x_continuous(sec.axis = ggplot2::sec_axis(~. / nops_4MP * 1000.0, name = 'Scaling factor', labels = scales::label_percent())) +
  # scale_color_manual(name='Health Outcome', values=erf_colors, breaks=names(comp)[3:nstats]) +
  labs(title = 'Projected population health impacts from annual airfield operations', x = 'Annual airfield operations (thousands)', y = 'Estimated population impacted (thousands)', color = 'Health outcome') + 
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    text=element_text(size=20),
    legend.position = 'none'
  ); p_simulations
ggsave(p_simulations, file=paste0('figures/_output/simulations.png'), width=12, height=10)
