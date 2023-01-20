events_data = read.csv('data/events/Public_NoiseEvents_NASWI_M1.pdf.csv')
events_data$OnsetRate = NA # Add onset rate column
events_data$UniqueEventID = seq(1, nrow(events_data)) # Add a unique event ID

source('data/load_site_date.R')

id = '2B_T'
site_events = events_data[events_data$SiteID==id,]

# For each date at the site, calculate the onset rate of all events
dates = unique(as.Date(site_events$StartTime))
for (i in 1:length(dates)) {
  data = load_site_date(id, dates[i])
  
  date_events = site_events[as.Date(site_events$StartTime)==dates[i],]
  for (j in 1:length(date_events)) {
    event = date_events[j,]
    print(paste('EVENT', event$UniqueEventID))
    event_start = which(data$Time==as.POSIXct(event$StartTime[1], tz='UTC'))
    event_end = which(data$Time==as.POSIXct(event$StopTime[1], tz='UTC')) - 1
    event_data = data[event_start:event_end,]
    
    # print(paste('start', event_start, 'end', event_end))

    if (max(event_data$LAeq) != event$LAeq_Lmax) {
      warning(paste('The raw data LAeq max', max(event_data$LAeq),'does not match the recorded event LAeq_Lmax', event$LAeq_Lmax'!')
    }

    onset_end = which(event_data$LAeq==max(event_data$LAeq))[1]
    # print(paste('onset end', onset_end))
    onset_rate = (event_data$LAeq[onset_end]-event_data$LAeq[1])/(onset_end-1) # dBA per sec

    print(paste('OnsetRate is', onset_rate))
    events_data[events_data$UniqueEventID==event$UniqueEventID,'OnsetRate'] = onset_rate
    # print(events_data[events_data$UniqueEventID==event$UniqueEventID,])
  }
}

# event = site_events[1,] # just look at the first event for now
# event_time_start = event$StartTime[1]
# date = as.Date(event_time_start)
# 
# data = load_site_date(id, date)
# start = which(data$Time==as.POSIXct(event$StartTime[1], tz='UTC'))
# end = which(data$Time==as.POSIXct(event$StopTime[1], tz='UTC')) - 1
# event_data = data[start:end,]
# 
# if (max(event_data$LAeq) != event$LAeq_Lmax) {
#   warning('The raw data LAeq max does not match the recorded event LAeq_Lmax!')
# }
# 
# end_onset = which(event_data$LAeq==max(event_data$LAeq))
# onset_rate = (event_data$LAeq[end_onset]-event_data$LAeq[1])/(end_onset-1) # dBA per sec
