#### Extract noise events from pdfs and save to csv files
source('global.R')
library(pdftools)

pdf = '~/../../Volumes/gioj/PHI/NAVY/Aircraft Noise Event Database/PUBLIC_NASWI_NoiseEvents/Public_NoiseEvents_OlympicMOA_2020-10-20_to_2021-10-20.pdf'

format_date = '%m/%d/%Y'
format_time = '%H:%M:%S'

# Convert to data frame and export to csv
message(paste('Reading', basename(pdf), '...'))

event_pages = pdf_text(pdf)
ncols = 11
data = data.frame(matrix(ncol=ncols, nrow=0))
colnames(data) = c(scan(text=trimws(strsplit(event_pages[1],'\n')[[1]]), what ='', quiet=T)[1:ncols])

# For each page of the pdf, parse and bind to data
for (p in 1:length(event_pages)) {
  msg('Processing page', p)
  page = event_pages[p]
  page = strsplit(page, "\n")
  page = page[[1]]
  page = trimws(page)
  
  page_data = page[startsWith(page, 'E2')]
  for (line in page_data) {
    entries = strsplit(line, ' +')[[1]]
    if (length(entries) == 13) {
      # is aircraft
      data = rbind(data.frame(
        EventID = entries[1], # EventID
        StartTime = paste(entries[2], entries[3]), # StartTime
        StopTime = paste(entries[4], entries[5]), # StopTime
        DurationInSeconds = entries[6], # DurationInSeconds
        LAeq_SEL = as.numeric(entries[7]), # LAeq_SEL
        LAeq_Leq = as.numeric(entries[8]), # LAeq_Leq
        LAeq_Lmax = as.numeric(entries[9]), # LAeq_Lmax
        LAeq_LmaxTime = paste(entries[10], entries[11]), # LAeq_LmaxTime
        ActiveInactive = entries[12], # ActiveInactive
        IsNonAircraft = FALSE, # IsNonAircraft
        RemovedFromAnalysis = entries[13] # RemovedFromAnalysis
      ), data)
    } else if (length(entries) == 14) {
      # is non aircraft
    } else {
      stop(paste('Error parsing', length(entries), 'entries'))
    }
  }
}

data = data[data$RemovedFromAnalysis==FALSE, ]

head(data[
  order(data[,'LAeq_Lmax'], decreasing = T),
])

# TODO: Save to file
write.csv(data, 'data/events/_output/navy_reported_moa_events.csv')
