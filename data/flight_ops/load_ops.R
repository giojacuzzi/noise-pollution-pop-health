#### Extract ops from pdfs and save to csv files

# Ault Field -------------------------------------------------------------------

library(pdftools)
files = list.files(path='~/../../Volumes/SAFS Work/NAVY/Flight Operations Data/AultField_and_OlympicMOA/PUBLIC_NASWI_Operations_AultField_and_OlympicMOA/Ault Field/', pattern="*.pdf", full.names=T, recursive=F)

format_date = '%m/%d/%y'
format_time = '%H:%M'
ncols = 10

for (pdf in files) {
  pages = pdf_text(pdf)
  data = data.frame(matrix(ncol=ncols, nrow=0))
  colnames(data) = scan(text = trimws(strsplit(pages[1],'\n')[[1]]), what = "")[1:ncols]
  
  # Parse and bind each page to `data`
  for (page in pages) {
    
    page = strsplit(page, '\n')
    page = page[[1]]
    page = trimws(page)
    
    page_data = data.frame()
    page_text = scan(text = page, what = '')
    
    # Parse values from each row and bind it to the data frame
    row_indices = which(grepl('_U', page_text))
    logtime_indices = which(grepl(':', page_text))
    curr_row = 1
    for (row_idx in row_indices) {
      
      # For now, just find the LogTimes
      date = page_text[logtime_indices[curr_row]-1]
      time = page_text[logtime_indices[curr_row]]
      row_data = data.frame(
        OperationID = page_text[row_idx],
        LogTime     = as.POSIXct(paste(date, time), paste(format_date,format_time), tz='UTC')
      )
      curr_row = curr_row + 1
      
      # To scrape more data, follow below. Note that there are edge cases with multi-word entries
      # if (curr_row < length(row_indices)) {
      #   num_entries = row_indices[curr_row+1] - row_idx
      #   curr_row = curr_row + 1
      # } else {
      #   num_entries = length(page_text) - row_idx + 1
      # }
      # entries = page_text[row_idx:(row_idx+(num_entries-1))]
      # if (num_entries > 10) { # Edge case if notes are present
      #   entries = c(entries[1:9],entries[length(entries)])
      # }
      # row_data = data.frame(
      #   OperationID = entries[1],
      #   Runway      = entries[2],
      #   Aircraft    = entries[3],
      #   SubType     = entries[4],
      #   Type        = entries[5],
      #   Fix         = entries[6],
      #   Distance    = entries[7],
      #   LogTime     = as.POSIXct(paste(entries[8],entries[9]), paste(format_date,format_time), tz='UTC'),
      #   TrackID     = entries[10]
      # )
      
      page_data = rbind(page_data, row_data)
    }
    data = rbind(data, page_data)
  }
  
  filename = paste0('data/flight_ops/output/ault/', basename(pdf), '.csv')
  write.csv(data, filename, row.names=F)
  print(paste('Created', filename))
}