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

results_long = results %>% pivot_longer(cols=names(results)[3:(ncol(results)-2)], names_to='Outcome', values_to='Estimate')

p_simulations = results_long %>% ggplot(aes(x=Operations, y=Estimate, group=Outcome, color=Outcome)) +
  geom_vline(aes(xintercept=nops_4MP), linetype = 'dashed', color = 'gray') +
  geom_vline(aes(xintercept=nops_A2A), linetype = 'dashed', color = 'gray') +
  geom_vline(aes(xintercept=nops_NA), linetype = 'dashed', color = 'gray') +
  annotate('text', x=nops_4MP - 3000, y=50000, label='This study', angle=90, color = 'gray') +
  annotate('text', x=nops_NA - 3000, y=50000, label='No Action Alternative', angle=90, color = 'gray') +
  annotate('text', x=nops_A2A - 3000, y=50000, label='Projected 2021 Total', angle=90, color = 'gray') +
  geom_line() +
  geom_point() +
  scale_x_continuous(sec.axis = ggplot2::sec_axis(~. / nops_4MP, name = 'Scaling Factor', labels = scales::label_percent())) +
  # scale_color_manual(name='Health Outcome', values=erf_colors, breaks=names(comp)[3:nstats]) +
  labs(title = 'Projected population health impacts from annual airfield operations', x = 'Total Annual Airfield Operations', y = 'Estimated Population Impacted', color = 'Health Outcome'); p_simulations
ggsave(p_simulations, file=paste0('figures/_output/simulations.png'), width=ggsave_width, height=ggsave_height)
