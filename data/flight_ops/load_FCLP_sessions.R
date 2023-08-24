# Extract sessions from FCLP Time Periods
library(pdftools)
library(stringr)
source('global.R')

pdf = paste0(database_path, '/NAVY/Flight Operations Data/OLFCoupeville/PUBLIC_NASWI_Operations_OLFCoupeville/OLF_FCLP_TimePeriods.pdf')
pages = pdf_text(pdf)

fulltext = data.frame()
for (page in pages) {
  page = strsplit(page, '\n')
  page = page[[1]]
  page = trimws(page)
  fulltext = rbind(fulltext, data.frame(page))
}


fclps = data.frame()
l = 4 # Skip scrapped session
while (l <= 37) {
  if (!(l %in% c(9,10))) { # Skip unapplicable session
    line = fulltext[l,1]
    line = strsplit(line, ' +')[[1]]
    # print(l)
    print(line)
    
    fclps = rbind(fclps, c(line))
  }
  l = l + 1
}
names(fclps) = c('Period', 'Date', 'Start', 'Stop', 'Duration')

fclps$Start = as.POSIXct(paste(fclps$Date, fclps$Start), tz='UTC', format='%m/%d/%Y %H:%M')
fclps$Stop = as.POSIXct(paste(fclps$Date, fclps$Stop), tz='UTC', format='%m/%d/%Y %H:%M')
fclps$Duration = as.numeric(fclps$Stop - fclps$Start)

message('FCLP session statistics (minutes): ')
print(summary(fclps$Duration))
