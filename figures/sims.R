sim_4MP = read.csv('analysis/_output/health_impact_summary.csv') # 2021 4-month monitoring period aggregate ops
sim_A2A = read.csv('analysis/_output/sims/health_impact_summary_Alternative2A.csv') # 2018 EIS Alternative 2A annual ops (2021 projection)
sim_NA = read.csv('analysis/_output/sims/health_impact_summary_PreExpansion.csv') # 2018 EIS No Action

nops_4MP = 93361.84
nops_A2A = 112100
nops_NA  = 84700

last_row = nrow(sim_4MP)
comp = rbind(
  sim_4MP[last_row,],
  sim_A2A[last_row,],
  sim_NA[last_row,]
)
comp$Sim = c('4MP', 'A2A', 'NA')
comp$Ops = c(nops_4MP, nops_A2A, nops_NA)

comp_long = comp %>% pivot_longer(cols=names(comp)[3:8], names_to='Outcome', values_to='Estimate')

comp_long %>% ggplot(aes(x=Ops, y=Estimate, group=Outcome, color=Outcome)) +
  geom_vline(aes(xintercept=nops_4MP), linetype = 'dashed', color = 'gray') +
  geom_vline(aes(xintercept=nops_A2A), linetype = 'dashed', color = 'gray') +
  geom_vline(aes(xintercept=nops_NA), linetype = 'dashed', color = 'gray') +
  geom_line() +
  geom_point()
