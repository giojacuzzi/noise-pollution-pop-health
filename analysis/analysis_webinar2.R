source('global.R')
library(mapview)
library(dplyr)
library(scales)

data_sites = get_data_sites()

data_metrics = get_data_metrics()
data_metrics$Field = unlist(lapply(data_metrics$ID, get_field_name_for_ID))

data_ops = get_data_ops()

# Monitoring site map  ---------------------------------------------------------

measured_data_sites = data_sites[data_sites$ID %in% unique(data_metrics$ID),]

mapviewOptions(legend.pos='bottomright')
mapview(
  measured_data_sites,
  xcol='Longitude', ycol='Latitude', zcol='Org',
  crs=4269, grid=FALSE, legend=TRUE,
  col.regions=c('navy', 'green3', 'darkturquoise'),
  # col.regions=c('sienna','gold','blue','green3','salmon','darkturquoise','purple3'),
  layer.name = 'Organization'
)

# Average Lden and total/field ops sentinel Whidbey sites all days -------------
# When is the navy active throughout the week, and how does it affect overall levels?

# Average ops across all 4 monitoring periods
mean_ops_day_ault = summary(data_ops[data_ops$Field=='Ault','Day']) # total
mean_ops_day_ault = sapply(mean_ops_day_ault, function(x){x/4}) # averaged
mean_ops_day_coup = summary(data_ops[data_ops$Field=='Coup','Day'])
mean_ops_day_coup = sapply(mean_ops_day_coup, function(x){x/4})

df_mean_ops = data.frame(
  Day   = factor(rep(days,2), levels=days),
  Field = factor(c(rep('Ault',7), rep('Coup',7))),
  Ops   = c(mean_ops_day_ault, mean_ops_day_coup)
)

sentinel_sites = c(
  '8B_SG', # Ault Field
  '24A_B'  # OLF Coupeville
)

sentinel_metrics_ault = data_metrics[data_metrics$ID==sentinel_sites[1],]
mean_lden_day_ault = tapply(X=sentinel_metrics_ault$Lden, INDEX=sentinel_metrics_ault$Day, FUN=mean)
sentinel_metrics_coup = data_metrics[data_metrics$ID==sentinel_sites[2],]
mean_lden_day_coup = tapply(X=sentinel_metrics_coup$Lden, INDEX=sentinel_metrics_coup$Day, FUN=mean)

df_mean_ops_lden = data.frame(
  Day   = factor(rep(days,2), levels=days),
  Field = factor(c(rep('Ault',7), rep('Coup',7))),
  Ops   = c(mean_ops_day_ault, mean_ops_day_coup),
  Lden  = c(mean_lden_day_ault, mean_lden_day_coup)
)

df_mean_ops_total = data.frame(
  Day = factor(days, levels=days),
  Ops = tapply(X=df_mean_ops_lden$Ops, INDEX=df_mean_ops_lden$Day, FUN=sum)
)

total_ops_field = tapply(df_mean_ops_lden$Ops, df_mean_ops_lden$Field, sum)

# TODO: change 0 ops days line to dashed style?
p_mean_ops_field = ggplot() +
  geom_line(data=df_mean_ops_lden, aes(x=Day, y=Ops, group=Field, color=Field), stat='identity') +
  labs(title='Average daily flight operations',
       subtitle=paste('Ault -', total_ops_field['Ault'], 'ops per week\nCoup -', total_ops_field['Coup'], 'ops per week'),
       x='',
       y='Operations')
p_mean_lden_field = ggplot() +
  geom_bar(data=df_mean_ops_lden, aes(x=Day, y=Lden, fill=Field), stat='identity', position='dodge', alpha=0.9) +
  # geom_line(data=df_mean_ops_total, aes(x=Day, y=((Ops/4+0))), group=1, size=1, color='black') +
  scale_y_continuous(name='Lden (dBA)', limits=c(50,90), oob=rescale_none) +
  labs(title='Average daily Lden',
       subtitle=paste('Sentinel sites 8B_SG and 24A_B'))
print(p_mean_ops_field / p_mean_lden_field)

# Lden per site and airfield on days of activity -------------------------------
# During days of activity, what are overall levels throughout the region?

days_ault_active = df_mean_ops_lden[df_mean_ops_lden$Field=='Ault' & df_mean_ops_lden$Ops > 0,]$Day
days_coup_active = df_mean_ops_lden[df_mean_ops_lden$Field=='Coup' & df_mean_ops_lden$Ops > 0,]$Day

# HUD and Federal Aviation Regulation Part 150 incompatible for
# residential land use
l_hudfaa = 65
# EPA recommended outdoor ambient noise level
l_epa = 55
# WHO Guideline 'strong' recommendation. Evidence for a relevant absolute risk
# of annoyance at 45 dB Lden was rated moderate quality.
l_who = 45

# NOTE: only look at Navy data for now
site_date_ldens = na.omit(data_metrics[,c('Org', 'Date', 'Day', 'Name', 'ID', 'Field', 'Lden')])
site_date_ldens = site_date_ldens[site_date_ldens$Org=='NAVY',]

active_site_date_ldens = rbind(
  site_date_ldens[site_date_ldens$Field=='Ault' & site_date_ldens$Day %in% days_ault_active,],
  site_date_ldens[site_date_ldens$Field=='Coup' & site_date_ldens$Day %in% days_coup_active,]
)

ggplot(active_site_date_ldens, aes(x=reorder(Name, Lden, FUN=median), y=Lden, fill=Field)) + 
  geom_boxplot(alpha=0.9) +
  labs(title='Lden per site, active days of operation', x ='Site', y ='Lden (dBA)') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_hline(yintercept=l_hudfaa, linetype='dotted', size=0.7, colour='red') +
  geom_hline(yintercept=l_epa, linetype='dotted', size=0.7, colour='red') +
  geom_hline(yintercept=l_who, linetype='dotted', size=0.7, colour='red') +
  coord_flip()

# Risk of high annoyance for days of average and maximum activity, all sites ---
# What is the risk of high annoyance at these sites based on exposure-response relationships?

# See ISO 1996-1 2016 Annex E/F and Lct

# The percent predicted to be highly annoyed in relation to exposure to aircraft traffic noise. Based on the WHO regression equation %HA = −50.9693 + 1.0168 × Lden + 0.0072 × Lden^2 derived from the systematic review (Guski et al., 2017).
# TODO: should be only defined for Lden [40, 75]
regression_WHO = function(Lden) {
  return(-50.9693 + 1.0168 * Lden + 0.0072 * Lden^2)
}

# Miedema and Oudshoorn 2001
regression_MO = function(Lden) {
  return(-9.199 * 10^-5 * (Lden - 42)^3 + 3.932 * 10^-2 * (Lden - 42)^2 + 0.2939 * (Lden - 42))
}

regression_ISO = function(Lden) {
  return(-9.199 * 10^-5 * (Lden - 40)^3 + 3.932 * 10^-2 * (Lden - 40)^2 + 0.294 * (Lden - 40))
}

# Median
median_lden = tapply(active_site_date_ldens$Lden, active_site_date_ldens$ID, median)
median_lden_HA = data.frame(
  Stat='Median',
  Lden=sort(median_lden),
  HA=regression_HA(sort(median_lden))
)

# Max
max_lden = tapply(active_site_date_ldens$Lden, active_site_date_ldens$ID, max)
max_lden_HA = data.frame(
  Stat='Max',
  Lden=sort(max_lden),
  HA=regression_HA(sort(max_lden))
)

combo = rbind(median_lden_HA, max_lden_HA)

p_ha = ggplot() +
  labs(x='Lden (dBA)', y='%HA', title='WHO - Percent Highly Annoyed') +
  xlim(40, max(combo$Lden)+5) +
  ylim(0, max(combo$HA)+5) +
  stat_function(fun=regression_WHO) +
  stat_function(fun=regression_MO, colour = 'magenta') +
  stat_function(fun=regression_ISO, colour= 'purple') +
  geom_point(
    data=combo,
    aes(x=Lden, y=HA, color=factor(Stat)),
    # label=rownames(median_lden_lnight_HSD),
    size=3
  ) +
  geom_hline(yintercept=100, linetype='dashed') +
  geom_vline(xintercept=75, linetype='dashed')
print(p_ha)
