source('global.R')

# https://www.osha.gov/laws-regs/regulations/standardnumber/1910/1910.95AppA#:~:text=(2)%20The%20eight%2Dhour,to%20the%20measured%20sound%20level.
refdur_osha = function(L) {
  return(8/(2^((L-90)/5))) # hours
}

# https://www.cdc.gov/niosh/docs/98-126/pdfs/98-126.pdf?id=10.26616/NIOSHPUB98126
refdur_niosh = function(L) {
  return(8/(2^((L-85)/3))) # hours
}

# ggplot() +
#   labs(title='Refdur vs Level') +
#   stat_function(fun=refdur_osha, color='blue') +
#   stat_function(fun=refdur_niosh, color='red') +
#   scale_x_continuous(limits = c(0, 100)) +
#   scale_y_continuous(limits = c(0, 100))

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

# ggplot() +
#   labs(title='TWA vs Dose') +
#   stat_function(fun=twa_osha, color='blue') +
#   stat_function(fun=twa_niosh, color='red') +
#   scale_x_continuous(limits = c(0, 100)) +
#   scale_y_continuous(limits = c(0, 100))

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

# # Dose via hourly Leqs
# data_metrics = get_data_metrics()
# for (i in 1:nrow(data_metrics)) {
#   test_date = data_metrics[i, which(colnames(data_metrics)=='Leq00'):which(colnames(data_metrics)=='Leq23')]
#   if (anyNA(test_date)) next
#   l = unlist(test_date)
#   c = rep(1, 24)
#   
#   t = refdur_osha(l)
#   d = round(100*sum(c/t),1)
#   osha_action_trigger = (twa_osha(d) >= 85)
#   if (osha_action_trigger) { # OSHA requires employers to implement a hearing conservation program when noise exposure is at or above 85 decibels averaged over 8 working hours
#     message(paste0('OSHA ', data_metrics[i,'ID'], ' ', data_metrics[i,'Date'], ' Lden ', data_metrics[i,'Lden'], ' dose ', d,'%, twa ', twa_osha(d)))
#   }
#   
#   t = refdur_niosh(l)
#   d = round(100*sum(c/t),1)
#   niosh_recommended_limitr_trigger = (twa_niosh(d) >= 85)
#   if (niosh_recommended_limitr_trigger) { # NIOSH recommended exposure limit (REL) is 85 decibels, A-weighted, as an 8-hr time-weighted average
#     message(paste0('NIOSH ', data_metrics[i,'ID'], ' ', data_metrics[i,'Date'], ' Lden ', data_metrics[i,'Lden'], ' dose ', d,'%, twa ', twa_niosh(d)))
#   }
# }

# # TODO: Evaluate FAA Hearing Conservation Program action level trigger
# # https://www.faa.gov/documentLibrary/media/Order/Order_3900.66A.pdf
# # "The AL or the TWA exposure which requires program inclusion is 82 dBA, or a dose of 50 percent. FS employees exposed to this level for 30 days or more per year require inclusion in the HCP."
# results[results$NioshTWA>=82,]


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
    # Org        = data_metrics[r,'Org'], # NOTE: non-Navy 
    LSeq24     = LeqTotal(l),
    OshaDose   = osha_d,
    OshaTWA    = osha_twa,
    OshaAction = osha_action_level,
    NioshDose  = niosh_d,
    NioshTWA   = niosh_twa,
    NioshLimit = niosh_recommended_limit
  ))
}

write.csv(results, 'data/metrics/_output/osha_niosh.csv', row.names=F)
