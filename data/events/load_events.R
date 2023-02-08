#### Extract noise events from pdfs and save to csv files

library(pdftools)
files = list.files(path='~/../../Volumes/SAFS Work/NAVY/Aircraft Noise Event Database/PUBLIC_NASWI_NoiseEvents', pattern="Public_NoiseEvents_NASWI_M(1|2|3|4).pdf", full.names=T, recursive=F)

format_date = '%m/%d/%Y'
format_time = '%H:%M:%S'

# For each pdf, convert to data frame and export to csv
for (pdf in files) {
  message(paste('Reading', basename(pdf), '...'))
  period = substring(basename(pdf), 27, 27) # Monitoring period
  
  event_pages = pdf_text(pdf)
  period_data = data.frame(matrix(ncol=9, nrow=0))
  colnames(period_data) = c('SiteID',scan(text=trimws(strsplit(event_pages[1],'\n')[[1]]), what ='', quiet=T)[1:8])
  
  # For each page of the pdf, parse and bind to period_data
  for (page in event_pages) {
    page = strsplit(page, "\n")
    page = page[[1]]
    page = trimws(page)
    
    page_data = data.frame(matrix(ncol=9, nrow=0))
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
  
  path = paste0('data/events/output/events_', period, '.csv')
  write.csv(period_data, path, row.names=F)
  message(paste('Created', path))
}