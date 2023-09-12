library(tidyr)

source('global.R')

sim_4MP = read.csv('analysis/_output/health_impact_summary.csv') # 2021 4-month monitoring period aggregate ops
sim_A2A = read.csv('analysis/_output/sims/health_impact_summary_Alternative2A.csv') # 2018 EIS Alternative 2A annual ops (2021 projection)
sim_NA = read.csv('analysis/_output/sims/health_impact_summary_PreExpansion.csv') # 2018 EIS No Action

nops_4MP = 93361.84
nops_A2A = 112100
nops_NA  = 84700

# 140,042.8 (50 % increase) *
# 130,000
# 120,000
# 112,100 ---- A2A
# 110,000
# 100,000
# 93,361.84 ---- 4MP
# 90,000
# 84,700 ---- No Action
# 80,000
# 46,680.92 (50 % decrease) *

last_row = nrow(sim_4MP)
comp = rbind(
  sim_4MP[last_row,],
  sim_A2A[last_row,],
  sim_NA[last_row,]
)

nstats = ncol(comp) - 1
comp$Exposed = as.numeric(comp$Exposed)
comp$Siulation = c('4MP', 'A2A', 'NA')
comp$Operations = c(nops_4MP, nops_A2A, nops_NA)

names(comp) = c('Name', 'Population', 'Exposed Population', 'Highly Annoyed (FICON)', 'Highly Annoyed (ISO)', 'Highly Annoyed (WHO)', 'Highly Annoyed (FAA NES)', 'Highly Annoyed (Yokoshima)', 'Highly Sleep Disturbed (WHO)', 'Highly Sleep Disturbed (Smith)', 'Hearing Loss and Cardiovascular Effects (EPA)', 'Simulation', 'Operations')
# outcome_colors = c('yellow', 'royalblue', 'darkorchid2', 'black', 'forestgreen', 'red')
# names(outcome_colors) = names(comp)[3:nstats]

comp_long = comp %>% pivot_longer(cols=names(comp)[3:nstats], names_to='Outcome', values_to='Estimate')

p_simulations = comp_long %>% ggplot(aes(x=Operations, y=Estimate, group=Outcome, color=Outcome)) +
  geom_vline(aes(xintercept=nops_4MP), linetype = 'dashed', color = 'gray') +
  geom_vline(aes(xintercept=nops_A2A), linetype = 'dashed', color = 'gray') +
  geom_vline(aes(xintercept=nops_NA), linetype = 'dashed', color = 'gray') +
  geom_line() +
  geom_point() +
  # scale_color_manual(name='Health Outcome', values=erf_colors, breaks=names(comp)[3:nstats]) +
  labs(title = 'Projected population health impacts from annual airfield operations', x = 'Total Annual Airfield Operations', y = 'Estimated Population Impacted', color = 'Health Outcome'); p_simulations
ggsave(p_simulations, file=paste0('figures/_output/simulations.png'), width=ggsave_width, height=ggsave_height)
