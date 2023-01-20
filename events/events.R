events_data = read.csv('data/events/Public_NoiseEvents_NASWI_M1.pdf.csv')

source('data/load_site_date.R')

id = '2B_T'
site_events = events_data[events_data$SiteID==id,]
event = site_events[1,] # just look at the first event for now
event_time_start = event$StartTime[1]
date = as.Date(event_time_start)

data = load_site_date(id, date)
start = which(data$Time==as.POSIXct(event$StartTime[1], tz='UTC'))
end = which(data$Time==as.POSIXct(event$StopTime[1], tz='UTC')) - 1
event_data = data[start:end,]

if (max(event_data$LAeq) != event$LAeq_Lmax) {
  warning('The raw data LAeq max does not match the recorded event LAeq_Lmax!')
}

end_onset = which(event_data$LAeq==max(event_data$LAeq))
onset_rate = (event_data$LAeq[end_onset]-event_data$LAeq[1])/(end_onset-1) # dBA per sec
