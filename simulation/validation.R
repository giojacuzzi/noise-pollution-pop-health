## Validate output of simulation against real-world measurements and previous navy modeling

source('global.R')
source('metrics/metrics.R')
library(patchwork)

data_sites   = get_data_sites()
data_metrics = get_data_metrics()
data_metrics$Field = unlist(lapply(data_metrics$ID, get_field_name_for_ID))

navy_metrics = na.omit(data_metrics)
navy_metrics = navy_metrics[navy_metrics$Org=='NAVY',]

# Calculated Ldn vs Navy-measured DNL vs NOISEMAP model DNL --------------------

# Compare with real-time measured DNL from navy report
ldn_metrics = navy_metrics[!is.na(navy_metrics$Ldn),]
avg_ldn = round(tapply(ldn_metrics$Ldn, ldn_metrics$ID, energyavg),1)
ldn_comparison = data.frame(ID=names(avg_ldn), Ldn=avg_ldn)
ldn_comparison$NavyDNL = NA
ldn_comparison[ldn_comparison$ID=='2B_T', 'NavyDNL']   = 64.8
ldn_comparison[ldn_comparison$ID=='3A_T', 'NavyDNL']   = 62.1
ldn_comparison[ldn_comparison$ID=='5B_SG', 'NavyDNL']  = 44.1
ldn_comparison[ldn_comparison$ID=='8B_SG', 'NavyDNL']  = 70.1
ldn_comparison[ldn_comparison$ID=='9B_SG', 'NavyDNL']  = 65.9
ldn_comparison[ldn_comparison$ID=='20B_SG', 'NavyDNL'] = 71.6
ldn_comparison[ldn_comparison$ID=='24A_B', 'NavyDNL']  = 84.3
ldn_comparison[ldn_comparison$ID=='25B_T', 'NavyDNL']  = 69.9
ldn_comparison[ldn_comparison$ID=='26B_SG', 'NavyDNL'] = 74.2
ldn_comparison[ldn_comparison$ID=='27A_SG', 'NavyDNL'] = 69.2
ldn_comparison[ldn_comparison$ID=='33_SG', 'NavyDNL']  = 39.9
# Compare with real-time modeled DNL from navy report, energy-averaged over each period's modeled DNL
ldn_comparison$NavyModeledDNL = NA
ldn_comparison[ldn_comparison$ID=='2B_T', 'NavyModeledDNL']   = 67.9
ldn_comparison[ldn_comparison$ID=='3A_T', 'NavyModeledDNL']   = 63.2
ldn_comparison[ldn_comparison$ID=='5B_SG', 'NavyModeledDNL']  = 44.7
ldn_comparison[ldn_comparison$ID=='8B_SG', 'NavyModeledDNL']  = 76.6
ldn_comparison[ldn_comparison$ID=='9B_SG', 'NavyModeledDNL']  = 73.1
ldn_comparison[ldn_comparison$ID=='20B_SG', 'NavyModeledDNL'] = 79.4
ldn_comparison[ldn_comparison$ID=='24A_B', 'NavyModeledDNL']  = 85.1
ldn_comparison[ldn_comparison$ID=='25B_T', 'NavyModeledDNL']  = 67.9
ldn_comparison[ldn_comparison$ID=='26B_SG', 'NavyModeledDNL'] = 77.5
ldn_comparison[ldn_comparison$ID=='27A_SG', 'NavyModeledDNL'] = 75.2
ldn_comparison[ldn_comparison$ID=='33_SG', 'NavyModeledDNL']  = 40.8
# Points of interest results taken from baseops simulation .poi
poi_metrics = readLines('simulation/baseops/DNL/NASWI_Combined_Average_DNL - Aggregate - NMap.poi')
start = which(str_detect(poi_metrics, ' POINT-ID')) + 1
poi_metrics = poi_metrics[start:(start+10)]
poi_metrics = data.frame(
  ID = sapply(strsplit(poi_metrics, ' +'), function(v) v[2]), # ID
  ModeledDNL = as.numeric(sapply(strsplit(poi_metrics, ' +'), function(v) tail(v, n = 1))) # TOTAL
)

ldn_comparison = full_join(ldn_comparison, poi_metrics, by = 'ID')

# NOTE: difference for Lopez (5B_SG) and PT (33_SG) is significant between PHI measured Ldn, potentially due to higher ambient noise from coastal wind, watercraft, and (in the case of PT) city activity. Note that the difference in Lden between these two sites during active and inactive days (below) is minimal, indicating a negligible effect of aircraft noise events on overall noise levels during this monitoring period. Therefore, we may choose to forgo including them in the site-specific health analysis.
comparison = data.frame(
  ID=factor(ldn_comparison$ID),
  Ldn_PhiMeasured    = ldn_comparison$Ldn,
  DNL_PhiModeled     = ldn_comparison$ModeledDNL,
  DNL_NavyMeasured   = ldn_comparison$NavyDNL,
  DNL_NavyModeled    = ldn_comparison$NavyModeledDNL,
  D_LdnPhiMeasured_DNLNavyMeasured = (ldn_comparison$Ldn - ldn_comparison$NavyDNL),
  D_LdnPhiMeasured_DNLPhiModeled  = (ldn_comparison$Ldn - ldn_comparison$ModeledDNL),
  D_DNLNavyMeasured_DNL_PhiModeled = (ldn_comparison$NavyDNL - ldn_comparison$ModeledDNL),
  D_DNLNavyModeled_DNL_PhiModeled = (ldn_comparison$NavyModeledDNL - ldn_comparison$ModeledDNL)
)
ggplot(gather(comparison, Source, Level, Ldn_PhiMeasured:DNL_NavyModeled, factor_key=T), aes(x = reorder(ID, Level, FUN=mean), y = Level, color = Source)) +
  geom_point(shape = 1)

# Navy model energy average vs Phi model
message('Navy modeled energy average vs simulation...')
p_modeled = ggplot(comparison, aes(x = DNL_NavyModeled, y = DNL_PhiModeled)) +
  geom_abline(slope=1, intercept=0, linetype='dashed') +
  geom_smooth(method='lm', color='black') +
  geom_point(comparison, mapping=aes(color=ID), size=3, pch=16) +
  labs(title='', x='Navy Modeled DNL (Energy Average)', y='Simulated DNL (Average Operations)', color = 'Site ID') +
  theme_bw()
message('Pearson correlation: ', cor(comparison$DNL_PhiModeled, comparison$DNL_NavyModeled, method='pearson'))
message('Difference statistics:')
print(summary(abs(comparison$DNL_PhiModeled - comparison$DNL_NavyModeled)))

# Navy measured vs Phi model
message('Navy measured energy average vs simulation...')
p_measured = ggplot(comparison, aes(x = DNL_NavyMeasured, y = DNL_PhiModeled)) +
  geom_abline(slope=1, intercept=0, linetype='dashed') +
  geom_smooth(method='lm', color='black') +
  geom_point(comparison, mapping=aes(color=ID), size=3, pch=16) +
  labs(title='', x='Navy Measured DNL (Energy Average)', y='Simulated DNL (Average Operations)', color = 'Site ID') +
  theme_bw()
message('Pearson correlation: ', cor(comparison$DNL_PhiModeled, comparison$DNL_NavyMeasured, method='pearson'))
message('Difference statistics:')
print(summary(abs(comparison$DNL_PhiModeled - comparison$DNL_NavyMeasured)))

p_validation = (p_modeled + p_measured + plot_layout(guides = 'collect'))
print(p_validation)

ggsave(p_validation, file=glue('simulation/_output/validation.png'), width=16, height=8)

# Explanation for using continuous Lden for Navy sites
# - We do not have the data or tools necessary to classify noise events of Navy sites. Other sites (JGL, NPS, and SDA) had in-person operators validating the presence of noise events due to aircraft operations
# - Aircraft operations produce the dominant noise events in the area, and their influence on overall level values far outweighs that of any transient noise*
# - Ldn values are within +/- 3 dB of Navy reported measured values for Navy sites (excluding Lopez and PT)
summary(comparison[!(comparison$ID %in% c('5B_SG', '33_SG')), 'D_LdnPhiMeasured_DNLNavyMeasured'])
# - Modeled DNL vaues are with +/- 10 dB of calculated Ldn values (excluding PT), mean absolute error 5.88 dB
summary(comparison[!(comparison$ID %in% c('5B_SG', '33_SG')), 'D_LdnPhiMeasured_DNLPhiModeled'])
# * This may not be the case for sites far from the airfields, specifically Lopez 5B_SG and Port Townsend 33_SG

# Energy average Lden
lden_metrics = data_metrics[!is.na(data_metrics$Lden),]
round(tapply(lden_metrics$Lden, lden_metrics$ID, energyavg),1)

# Olympic MOA monthly DNL during activity
moa_dnl = c(41.2, 43.1, 36.3, 35.3, 38.8, 34.4, 46.9, 35.7, 38.6, 34.3, 34.6, 35.4, 36.6)

