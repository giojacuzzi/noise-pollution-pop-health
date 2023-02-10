# Extract ops from 'Flight Operations Data' pdfs and save to csv files

# Ault Field -------------------------------------------------------------------
library(pdftools)

files = list.files(path='~/../../Volumes/SAFS Work/NAVY/Flight Operations Data/AultField_and_OlympicMOA/PUBLIC_NASWI_Operations_AultField_and_OlympicMOA/Ault Field/', pattern="*.pdf", full.names=T, recursive=F)

format_date = '%m/%d/%y'
format_time = '%H:%M'
ncols = 10

data_ault = data.frame()

for (pdf in files) {
  pages = pdf_text(pdf)
  data_period = data.frame(matrix(ncol=ncols, nrow=0))
  colnames(data_period) = scan(text = trimws(strsplit(pages[1],'\n')[[1]]), what = "")[1:ncols]

  # Parse and bind each page to `data_period`
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
        # ID = page_text[row_idx],
        Time     = as.POSIXct(paste(date, time), paste(format_date,format_time), tz='UTC')
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
    data_period = rbind(data_period, page_data)
  }
  
  data_ault = rbind(data_ault, data_period)
  # filename = paste0('data/flight_ops/output/ault/', basename(pdf), '.csv')
  # write.csv(data, filename, row.names=F)
  # message(paste('Created', filename))
}

# OLF Coupeville FCLP ----------------------------------------------------------
library(pdftools)

# NOTE: A session on 12/16/20 from ~14:30-15:10 was mistakenly not observed, and is not included

rm()
files = list.files(path='~/../../Volumes/SAFS Work/NAVY/Flight Operations Data/OLFCoupeville/PUBLIC_NASWI_Operations_OLFCoupeville/', pattern="*Ops.pdf", full.names=T, recursive=F)

ncols = 1

get_num_numeric_entries = function(row) {
  length(na.omit(suppressWarnings(as.numeric(scan(text=row, what='')))))
}

data_coup = data.frame()

for (pdf in files) {
  # readline(prompt=paste('Press [enter] to process', basename(pdf)))
  
  pages = pdf_text(pdf)
  
  date =  substring(basename(pdf), 1, 10)
  
  for (page in pages) {
    page = strsplit(page, '\n')
    page = page[[1]]
    page = trimws(page)
    
    page_data = data.frame()
    
    idx = grep('Stamp', page)
    if (length(idx) > 0) { # Header, skip this row
      # print(paste('found header at line', idx))
      # print(page[idx])
      idx = idx + 1
      
      if (grepl(date, '-06-') | grepl(date, '-08-')) {
        # Skip first line of these specific months
        idx = idx + 1
      }
    } else { # Data, read from top
      idx = 1
    }

    while (idx <= length(page)) {

      # Skip non-data rows
      while ((idx <= length(page)) &
             (!grepl(':', page[idx], ignore.case=T) |
               grepl('session', page[idx], ignore.case=T) |
              grepl('start at', page[idx], ignore.case=T) |
              grepl('start of', page[idx], ignore.case=T) |
              grepl('stop of', page[idx], ignore.case=T) |
              grepl('end of', page[idx], ignore.case=T) |
              grepl('none', page[idx], ignore.case=T))
             
             ) { # Start/stop line, skip
        # print(paste('skipping start/stop line:'))
        # print(page[idx])
        idx = idx + 1
      }
      
      # print(paste('reading from line', idx))
      row = page[idx]
      if (is.na(row)) {
        idx = idx + 1
        next
      }

      if (length(grep('AM', row))) {
        time = substr(row, 1, gregexpr('AM', row)[[1]][1] + 1)
      } else if (length(grep('PM', row))) {
        time = substr(row, 1, gregexpr('PM', row)[[1]][1] + 1)
      } else {
        # print('skipping no am/pm:')
        # print(row)
        idx = idx + 1
        next
      }
      
      timestamp = as.POSIXct(paste(date, time), paste('%Y-%m-%d','%I:%M:%S %p'), tz='UTC')
      
      # print('READING:')
      # print(row)
      # print(paste(time, '->', timestamp, 'with', get_num_numeric_entries(row), 'entries'))
      
      row_data = data.frame(
        Time = timestamp
      )
      page_data = rbind(page_data, row_data)

      idx = idx + 1
    }
    # print(paste('First', page_data[1,]))
    # print(paste('Last', page_data[nrow(page_data),]))
    
    data_coup = rbind(data_coup, page_data)
    message(paste('Read', date))
  }
}

data_coup = na.omit(data_coup)
# filename = paste0('data/flight_ops/output/coup/Coupeville Ops.csv')
# write.csv(data, filename, row.names=F)
# message(paste('Created', filename))

# TODO: bind coup and ault, save to csv
data_ault$Field = 'Ault'
data_coup$Field = 'Coupeville'
data = data.frame()
data = rbind(data, data_ault)
data = rbind(data, data_coup)
path = 'data/flight_ops/output/ops.csv'
write.csv(data, path, row.names=F)
message(paste('Created', path))
