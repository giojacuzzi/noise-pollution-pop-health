# Validate noise events for each site date, ensuring they are aircraft events
source('global.R')

data_events = get_data_events()
data_navy_events_reported = get_data_navy_events_reported()
data_events_navy_calculated = data_events[data_events$Org=='NAVY',]

# INSTEAD:
# For each site, and each date at that site, determine what noise events occur during an operation + the time of acoustic propagation from distance to airfield (+/- a buffer)

data_ops = get_data_ops()
data_sites = get_data_sites()

ault_lat  = 48.351803
ault_long = -122.65591
coup_lat  = 48.188345
coup_long = -122.632041

km_from_most_relevant_field = list(
  # Ault
  "5B_SG"=14.14,
  "9B_SG"=1.9,
  "8B_SG"=2.72,
  "2B_T" =6.52,
  "3A_T" =14.55,
  "33_SG"=27.20,
  # Coup
  "26B_SG"=3.48,
  "24A_B" =2.55,
  "27A_SG"=1.95,
  "20B_SG"=2.10,
  "25B_T" =3.56
)

# seconds of sound propagation delay
delay = sapply(km_from_most_relevant_field, function(km){km*1000/343})

library(data.table)
for (id in unique(data_events_navy_calculated$ID)) {

  site_events_calc = data_events_navy_calculated[data_events_navy_calculated$ID==id,]
  site_ops = data_ops[data_ops$Field==get_field_name_for_ID(id),]

  for (date in unique(site_events_calc$Date)) {
    date_events_calc = site_events_calc[site_events_calc$Date==date,]
    date_ops = site_ops[site_ops$Date==date,]

    print(paste(nrow(date_events_calc), 'calculated (', nrow(date_ops), 'ops ) events for date', date, 'at site', id))

    # for (i in 1:nrow(date_events_calc)) { # for each calculated event
    #   for (j in 1:nrow(date_events_rept)) { # for each reported event
    #     if(between(data_navy_events_reported$LAeq_LmaxTime[j],
    #             data_events_navy_calculated$TimeStart[i],
    #             data_events_navy_calculated$TimeEnd[i]) == T) {
    #       print(paste('yay', i, j))
    #     }
    #   }
    # }
  }


}









# library(data.table)
# # For each event in data_events, find an event in data_navy_events whose time_lmax falls within time_start and time_end
# data_events_navy_calculated$Validated = FALSE
# for (id in unique(data_events_navy_calculated$ID)) {
#   
#   site_events_calc = data_events_navy_calculated[data_events_navy_calculated$ID==id,]
#   site_events_rept = data_navy_events_reported[data_navy_events_reported$SiteID==id,]
#   
#   for (date in unique(site_events_calc$Date)) {
#     date_events_calc = site_events_calc[site_events_calc$Date==date,]
#     date_events_rept = site_events_rept[site_events_rept$Date==date,]
#     
#     print(paste(nrow(date_events_calc), 'calculated (of', nrow(date_events_rept), 'reported) events for date', date, 'at site', id))
#   
#     for (i in 1:nrow(date_events_calc)) { # for each calculated event
#       for (j in 1:nrow(date_events_rept)) { # for each reported event
#         if(between(data_navy_events_reported$LAeq_LmaxTime[j],
#                 data_events_navy_calculated$TimeStart[i],
#                 data_events_navy_calculated$TimeEnd[i]) == T) {
#           print(paste('yay', i, j))
#         }
#       }
#     }
#   }
#   
#   
# }
# # for () {
# #   between(data_navy_events_reported$LAeq_LmaxTime[1], data_events_navy_calculated$TimeStart[1], data_events_navy_calculated$TimeEnd[1])
# # }
# 
# 
# 
# # # TODO: for each event in data_events, find an event in data_navy_events whose time_lmax falls within time_start and time_end
# # 
# # library(lubridate)
# # library(plyr)
# # # data_events_navy_calculated_times = data_events_navy_calculated[,c('TimeStart','TimeEnd')]
# # # Interval <- as.interval(data_events_navy_calculated$TimeStart[1], data_events_navy_calculated$TimeEnd[1])
# # # data_navy_events_reported$LAeq_LmaxTime[1] %within% Interval
# # 
# # # Frequency = 1
# # library(data.table)
# # TimeStart  = data_events_navy_calculated$TimeStart
# # TimeEnd    = data_events_navy_calculated$TimeEnd
# # TimeMarker = as.POSIXct(data_navy_events_reported$LAeq_LmaxTime, tz='UTC')
# # 
# # traffic = data.frame(TimeMarker, Frequency=1)
# # 
# # 
# # 
# # us <- data.table(TimeStart, TimeEnd, key = c("TimeStart", "TimeEnd"))
# # them <- data.table(start = TimeMarker, end = TimeMarker, key = c("start","end"))
# # foverlaps(them, us, type="any", nomatch=0L)
# 
# # TODO: Validation of non-navy events being actual aircraft events (SDA and NPS, not JGL because of in-person validation)