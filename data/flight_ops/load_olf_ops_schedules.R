## Extract flight operations from OLF Operations Schedules

library(pdftools)
library(stringr)
source('global.R')

pdf = paste0(database_path, '/FCLP Notifications/OLF Schedules - 2021.pdf')
pages = pdf_text(pdf)

fulltext = data.frame()
for (page in pages) {
  page = strsplit(page, '\n')
  page = page[[1]]
  page = trimws(page)
  fulltext = rbind(fulltext, data.frame(page))
}

coup_ops = data.frame()
ault_ops = data.frame()

delimiter = '|'

l = 1
while (l <= nrow(fulltext)) {
  line = fulltext[l,1]
  
  if (line == 'Coupeville carrier operations') {
    message('Coup!')
    while (l <= nrow(fulltext) & !grepl('Time Frame', line, fixed=T)) {
      l = l + 1
      line = fulltext[l,1]
      # message('moving...')
    }
    if(grepl('Time Frame', line, fixed=T)) {
      # message('Time Frame----------------')
      while (l <= nrow(fulltext) & !grepl('Ault Field', line, fixed=T)) {
        if (line != '' & !grepl('Time Frame', line, fixed=T) & !grepl('None', line, fixed=T)) {
          coup_ops = rbind(coup_ops, gsub("\\s{2,}", delimiter, str_trim(line)))
        }
        l = l + 1
        line = fulltext[l,1]
      }
    }
  }
  if (line == 'Ault Field carrier operations') {
    message('Ault!')
    while (l <= nrow(fulltext) & !grepl('Time Frame', line, fixed=T)) {
      l = l + 1
      line = fulltext[l,1]
      # message('moving...')
    }
    if(grepl('Time Frame', line, fixed=T)) {
      # message('Time Frame----------------')
      while (l <= nrow(fulltext) & !grepl('The FCLP', line, fixed=T)) {
        if (line != '' & !grepl('Time Frame', line, fixed=T) & !grepl('None', line, fixed=T)) {
          ault_ops = rbind(ault_ops, gsub("\\s{2,}", delimiter, str_trim(line)))
        }
        l = l + 1
        line = fulltext[l,1]
      }
    }
  }
  l = l + 1
}

names(coup_ops) = c('Delimited')
names(ault_ops) = c('Delimited')

# Combine trailing entries that took more than one line
coup_trailing = which(!grepl('|', coup_ops$Delimited, fixed=T))
for (i in coup_trailing) {
  coup_ops$Delimited[i-1] = paste(coup_ops$Delimited[i-1],coup_ops$Delimited[i])
}
coup_ops = data.frame(coup_ops[-coup_trailing,])

ault_trailing = which(!grepl('|', ault_ops$Delimited, fixed=T))
for (i in ault_trailing) {
  ault_ops$Delimited[i-1] = paste(ault_ops$Delimited[i-1],ault_ops$Delimited[i])
}
ault_ops = data.frame(ault_ops[-ault_trailing,])

names(coup_ops) = c('Delimited')
names(ault_ops) = c('Delimited')

# # Make data frames
coup_ops_final = data.frame()
for (i in 1:nrow(coup_ops)) {
  str = coup_ops$Delimited[i]
  op = data.frame(
    Date=substr(str, 1, which(strsplit(str, '')[[1]]==delimiter)-1),
    Periods=substr(str, which(strsplit(str, '')[[1]]==delimiter)+1, nchar(str))
  )
  coup_ops_final = rbind(coup_ops_final, op)
}
ault_ops_final = data.frame()
for (i in 1:nrow(ault_ops)) {
  str = ault_ops$Delimited[i]
  op = data.frame(
    Date=substr(str, 1, which(strsplit(str, '')[[1]]==delimiter)-1),
    Periods=substr(str, which(strsplit(str, '')[[1]]==delimiter)+1, nchar(str))
  )
  ault_ops_final = rbind(ault_ops_final, op)
}
