## Utility functions to assist with the calculation of OSHA and NIOSH standards measurements

source('global.R')

# https://www.osha.gov/laws-regs/regulations/standardnumber/1910/1910.95AppA#:~:text=(2)%20The%20eight%2Dhour,to%20the%20measured%20sound%20level.
refdur_osha = function(L) {
  return(8/(2^((L-90)/5))) # hours
}

# https://www.cdc.gov/niosh/docs/98-126/pdfs/98-126.pdf?id=10.26616/NIOSHPUB98126
refdur_niosh = function(L) {
  return(8/(2^((L-85)/3))) # hours
}

# Level to achieve 100% dose for duration t in hours
level_from_refdur_osha = function(t) {
  5*log2(8/t) + 90
}
message(paste0('24-hr level for 100% OSHA dose is ', level_from_refdur_osha(24)))

level_from_refdur_niosh = function(t) {
  3*log2(8/t) + 85
}
message(paste0('24-hr level for 100% NIOSH dose is ', level_from_refdur_niosh(24)))

# 8-hr TWA
# Action level reached at 85 dB TWA, legally requiring hearing protection program
twa_osha = function(d) {
  16.61 * log10(d/100) + 90
}

# 8-hr TWA
twa_niosh = function(d) {
  10 * log10(d/100) + 85
}

dose_osha = function(l, c) {
  t = refdur_osha(l)
  return(100*sum(c/t))
}

dose_niosh = function(l, c) {
  t = refdur_niosh(l)
  return(100*sum(c/t))
}

# When the sound level, L, is constant over the entire work shift, the noise dose, D, in percent, is given by:
l = 90 # measured A-weighted sound level
t = refdur_osha(l) # reference duration corresponding to l
c = 8 # length of workday
d = 100 * c / t
message(paste0(d,'%'))

# When the workshift noise exposure is composed of two or more periods of noise at different levels, the total noise dose over the work day is given by:
l = c(55,85,90,95)
c = c(16.5,6,1,0.5) # total time of exposure at a specific noise level
t = refdur_osha(l) # reference duration for that level
d = 100*sum(c/t)
message(paste0(d,'%'))

source('data/load/load_site_date.R')
source('metrics/metrics.R')

data_metrics = get_data_metrics()
results = data.frame()
for (r in 1:nrow(data_metrics)) {
  message(paste('Site date',r,'of',nrow(data_metrics)))
  
  id = data_metrics[r,'ID']
  date = data_metrics[r,'Date']
  test_date = load_site_date(id, as.Date(as.character(date)))
  test_date = na.omit(test_date)
  
  l = test_date$LAeq # NOTE: slow time-weighted levels expected
  c = rep(1/3600, length(l))
  
  t = refdur_osha(l)
  osha_d = round(100*sum(c/t),1)
  osha_twa = twa_osha(osha_d)
  message(paste0('OSHA  dose ', osha_d,'%', ' TWA ', round(osha_twa,1), 'dB'))
  osha_action_level = (osha_twa >= 85)
  if (osha_action_level) {
    message(paste0('OSHA  trigger ', osha_action_level))
  }
  
  t = refdur_niosh(l)
  niosh_d = round(100*sum(c/t),1)
  niosh_twa = twa_niosh(niosh_d)
  message(paste0('NIOSH dose ', niosh_d,'%', ' TWA ', round(niosh_twa,1), 'dB'))
  niosh_recommended_limit = (niosh_twa >= 85)
  if (niosh_recommended_limit) {
    message(paste0('NIOSH  trigger ', niosh_recommended_limit))
  }
  
  results = rbind(results, data.frame(
    Date       = date,
    ID         = id,
    LSeq24     = LeqTotal(l),
    OshaDose   = osha_d,
    OshaTWA    = osha_twa,
    OshaAction = osha_action_level,
    NioshDose  = niosh_d,
    NioshTWA   = niosh_twa,
    NioshLimit = niosh_recommended_limit
  ))
}

path = 'analysis/population_health_impacts/preprocessing/_output/osha_niosh.csv'
write.csv(results, path, row.names=F)
msg('Created', path)
