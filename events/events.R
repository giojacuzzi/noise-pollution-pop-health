events_data = read.csv('data/events/Public_NoiseEvents_NASWI_M1.pdf.csv')
events_data$OnsetRateRaw = NA # Add onset rate column (calculated from raw LAeq_Lmax values)
events_data$OnsetRateReported = NA # Add onset rate column (calculated from reported LAeq_Lmax values in pdfs)
events_data$UniqueEventID = seq(1, nrow(events_data)) # Add a unique event ID

source('data/load_site_date.R')

id = '2B_T'
site_events = events_data[events_data$SiteID==id,]

dates = as.character(unique(c(as.Date(site_events$StartTime),as.Date(site_events$StopTime))))

data = data.frame()
for (date in dates) {
  data = rbind(data, load_site_date(id, date))
}

results = events_data

# For all events at this site, calculate onset rate
for (row in 1:nrow(site_events)) {
  event = site_events[row,]
  print(paste('EVENT', event$UniqueEventID))
  
  event_start = which(data$Time==as.POSIXct(event$StartTime[1], tz='UTC'))
  event_end = which(data$Time==as.POSIXct(event$StopTime[1], tz='UTC')) - 1
  event_data = data[event_start:event_end,]
  # print(event_data$LAeq)
  if (length(event_data$LAeq) != event$DurationInSeconds) {
    print(event_data$LAeq)
    warning(paste(length(event_data$LAeq), '!=', event$DurationInSeconds))
  }


  if (max(event_data$LAeq) != event$LAeq_Lmax) {
    warning(paste('The raw data LAeq max', max(event_data$LAeq),'does not match the recorded event LAeq_Lmax', event$LAeq_Lmax, '! But does this', max(event_data$LApeak), '?'))
  }
  
  # Using event record Lmax
  their_onset_time_diff = as.numeric(as.POSIXct(event$LAeq_LmaxTime, tz='UTC') - as.POSIXct(event$StartTime, tz='UTC'))
  their_onset_rate = (event$LAeq_Lmax - event_data$LAeq[1]) / their_onset_time_diff
  their_onset_rate = round(their_onset_rate, 2)

  # Using only raw data, not the event records
  onset_end = which(event_data$LAeq==max(event_data$LAeq))[1]
  # if (onset_end == 1) { # First value is the max. Erroneous event start recording, so push back one second
  #   warning('First value is the max. Erroneous event start recording, so push back one second.')
  #   event_data = data[(event_start-1):event_end,]
  # }
  # NOTE: if onset_end == 1, the recorded event start is erroneous, and the onset will be NaN and discarded
  onset_rate = (event_data$LAeq[onset_end]-event_data$LAeq[1])/(onset_end-1) # dBA per sec
  onset_rate = round(onset_rate, 2)
  
  print(paste('OnsetRate is raw', onset_rate, 'vs theirs', their_onset_rate))
  print(paste(event_data$LAeq[onset_end], '-', event_data$LAeq[1], '/', onset_end,'-',1))
  print(paste(event$LAeq_Lmax, '-', event_data$LAeq[1], '/', their_onset_time_diff))

  results[results$UniqueEventID==event$UniqueEventID,'OnsetRateRaw'] = onset_rate
  results[results$UniqueEventID==event$UniqueEventID,'OnsetRateReported'] = their_onset_rate
}

# # For each date at the site, calculate the onset rate of all events
# dates = unique(as.Date(site_events$StartTime))
# for (i in 1:length(dates)) {
#   data = load_site_date(id, dates[i])
#   
#   date_events = site_events[as.Date(site_events$StartTime)==dates[i],]
#   for (j in 1:length(date_events)) {
#     event = date_events[j,]
#     print(paste('EVENT', event$UniqueEventID))
#     event_start = which(data$Time==as.POSIXct(event$StartTime[1], tz='UTC'))
#     event_end = which(data$Time==as.POSIXct(event$StopTime[1], tz='UTC')) - 1
#     event_data = data[event_start:event_end,]
#     
#     # print(paste('start', event_start, 'end', event_end))
# 
#     if (max(event_data$LAeq) != event$LAeq_Lmax) {
#       warning(paste('The raw data LAeq max', max(event_data$LAeq),'does not match the recorded event LAeq_Lmax', event$LAeq_Lmax'!')
#     }
# 
#     onset_end = which(event_data$LAeq==max(event_data$LAeq))[1]
#     # print(paste('onset end', onset_end))
#     onset_rate = (event_data$LAeq[onset_end]-event_data$LAeq[1])/(onset_end-1) # dBA per sec
# 
#     print(paste('OnsetRate is', onset_rate))
#     events_data[events_data$UniqueEventID==event$UniqueEventID,'OnsetRate'] = onset_rate
#     # print(events_data[events_data$UniqueEventID==event$UniqueEventID,])
#   }
# }
# 
# # event = site_events[1,] # just look at the first event for now
# # event_time_start = event$StartTime[1]
# # date = as.Date(event_time_start)
# # 
# # data = load_site_date(id, date)
# # start = which(data$Time==as.POSIXct(event$StartTime[1], tz='UTC'))
# # end = which(data$Time==as.POSIXct(event$StopTime[1], tz='UTC')) - 1
# # event_data = data[start:end,]
# # 
# # if (max(event_data$LAeq) != event$LAeq_Lmax) {
# #   warning('The raw data LAeq max does not match the recorded event LAeq_Lmax!')
# # }
# # 
# # end_onset = which(event_data$LAeq==max(event_data$LAeq))
# # onset_rate = (event_data$LAeq[end_onset]-event_data$LAeq[1])/(end_onset-1) # dBA per sec
