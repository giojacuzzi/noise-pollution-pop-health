#### Extract noise events from pdfs and save to csv files
source('global.R')
library(pdftools)

pdf = '~/../../Volumes/SAFS Work/PHI/NAVY/Aircraft Noise Event Database/PUBLIC_NASWI_NoiseEvents/Public_NoiseEvents_OlympicMOA_2020-10-20_to_2021-10-20.pdf'

format_date = '%m/%d/%Y'
format_time = '%H:%M:%S'

data = data.frame()

# Convert to data frame and export to csv
message(paste('Reading', basename(pdf), '...'))

event_pages = pdf_text(pdf)
ncols = 11
period_data = data.frame(matrix(ncol=ncols, nrow=0))
colnames(period_data) = c(scan(text=trimws(strsplit(event_pages[1],'\n')[[1]]), what ='', quiet=T)[1:ncols])

# For each page of the pdf, parse and bind to period_data
for (page in event_pages) {
  page = strsplit(page, "\n")
  page = page[[1]]
  page = trimws(page)
  
  page_data = data.frame(matrix(ncol=ncols, nrow=0))
  colnames(page_data) = colnames(period_data)
  page_text = scan(text=page, what ='', quiet=T)
  
  # Determine the site ID
  siteID = page_text[grep('Page', page_text) - 1]
  
  # Parse values for each row and bind it to the data frame
  ch = 9
  numStrPerRow = 11
  while (ch < length(page) * numStrPerRow) {
    row_text = page_text[ch:(ch+10)] # Row data
    row_data = list(
      siteID, # SiteID
      row_text[1], # EventID
      as.POSIXlt(paste(row_text[2],row_text[3]), paste(format_date,format_time), tz='UTC'), # StartTime (date and time)
      as.POSIXlt(paste(row_text[4],row_text[5]), paste(format_date,format_time), tz='UTC'), # StopTime (date and time)
      as.numeric(row_text[6]), # DurationInSeconds
      as.numeric(row_text[7]), # LAeq_SEL
      as.numeric(row_text[8]), # LAeq_Leq
      as.numeric(row_text[9]), # LAeq_max
      as.POSIXlt(paste(row_text[10],row_text[11]), paste(format_date,format_time), tz='UTC') # LAeq_LmaxTime (date and time)
    )
    row_data = as.data.frame(row_data)
    colnames(row_data) = colnames(period_data)
    page_data = rbind.data.frame(page_data, row_data)
    ch = ch + 11
  }
  colnames(page_data) = c('SiteID',page_text[1:8])
  page_data = na.omit(page_data)
  period_data = rbind(period_data, page_data)
  message(siteID)
}
period_data$Period = period
data = rbind(data, period_data)

# path = paste0('data/events/_output/events_', period, '.csv')
# write.csv(period_data, path, row.names=F)
# message(paste('Created', path))
}