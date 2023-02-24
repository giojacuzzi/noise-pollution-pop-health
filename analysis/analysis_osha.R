source('global.R')

# https://www.osha.gov/laws-regs/regulations/standardnumber/1910/1910.95AppA#:~:text=(2)%20The%20eight%2Dhour,to%20the%20measured%20sound%20level.
reference_duration = function(L) {
  return(8/(2^((L-90)/5)))
}

# Level to achieve 100% dose for duration t in hours
level_from_reference_duration = function(t) {
  5*log2(8/t) + 90
}
message(paste0('24-hr level for 100% dose is ', level_from_reference_duration(24)))

# 8-hr TWA
# Action level reached at 85 dB TWA
twa = function(d) {
  16.61*log10(d/100)+90
}

# When the sound level, L, is constant over the entire work shift, the noise dose, D, in percent, is given by:
l = 90 # measured A-weighted sound level
t = reference_duration(l) # reference duration corresponding to l
c = 8 # length of workday
d=100 * c / t
message(paste0(d,'%'))

# When the workshift noise exposure is composed of two or more periods of noise at different levels, the total noise dose over the work day is given by:
l = c(55,85,90,95)
c = c(16.5,6,1,0.5) # total time of exposure at a specific noise level
t = reference_duration(l) # reference duration for that level
d = 100*sum(c/t)
message(paste0(d,'%'))

# Dose via hourly Leqs
data_metrics = get_data_metrics()
for (i in 1:nrow(data_metrics)) {
  test_date = data_metrics[i, which(colnames(data_metrics)=='Leq00'):which(colnames(data_metrics)=='Leq23')]
  if (anyNA(test_date)) next
  
  l = unlist(test_date)
  c = rep(1, 24)
  t = reference_duration(l)
  d = round(100*sum(c/t),1)
  action_trigger = (twa(d) >= 85)
  if (action_trigger) { # OSHA requires employers to implement a hearing conservation program when noise exposure is at or above 85 decibels averaged over 8 working hours
    message(paste0(data_metrics[i,'Name'], ' ', data_metrics[i,'Date'], ' Lden ', data_metrics[i,'Lden'], ' dose ', d,'%, twa ', twa(d)))
  }
}

# TODO: Dose via second exposure per day
source('data/load/load_site_date.R')
test_date = load_site_date('24A_B', '2021-08-10')
l = test_date$LAS
c = rep(1/3600, length(l))
t = reference_duration(l)
d = round(100*sum(c/t),1)
message(paste0('Dose ', d,'%'))
