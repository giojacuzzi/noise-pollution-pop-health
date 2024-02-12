## Generate the per-period aggregates and the total combined average .csv outputs, respectively.
# Input: 'data/simulation/flight_ops/*.csv'
# Outputs:
# 'data/simulation/flight_ops/_output/NASWI_MP[N]_Noisemap - Flight Operations Aggregated.csv'
# 'data/simulation/flight_ops/_output/NASWI_Noisemap - Flight Operations Combined Average.csv'
# 'data/simulation/flight_ops/_output/NASWI_Noisemap - Flight Operations Combined Average - Night Only.csv'

library(dplyr)
source('global.R')

## Aggregate flight operations data per-period -------------------------------------------------------------------

clean_flight_ops_data = function(data) {
  data[data==''] = NA                                # Replace empty values with NA
  data = data[-which(data$Track.Group=='Subtotal'),] # Remove subtotal rows
  data = data[rowSums(is.na(data)) != ncol(data),]   # Remove empty (NA) rows
  data = data[-which(data$Aircraft=='Aircraft'),]    # Remove duplicate row headers
  data$Num.Day   = as.numeric(data$Num.Day)
  data$Num.Night = as.numeric(data$Num.Night)
  data$Num.Total = as.numeric(data$Num.Total)
  return(data.frame(data))
}

periods = c('MP1','MP2','MP3','MP4')

for (period in periods) {
  
  # Get all six files for the period...
  # NASWI_MP1_Noisemap - Coupeville, includes points-of-interest 20B_SG, 24A_B, 25B_T, 26B_SG, 27A_SG, and 33_SG
  # NASWI_MP1_02_Noisemap - 2B_T Seaplane Base
  # NASWI_MP1_03_Noisemap - 3A_T Skagit River Dike (WDFW Site)
  # NASWI_MP1_05_Noisemap - 5B_SG Lopez Island (Point Colville)
  # NASWI_MP1_08_Noisemap - 8B_SG Dog Park (North Whidbey Parks & Rec)
  # NASWI_MP1_09_Noisemap - 9B_SG NASWI Gate (Corner of Banta Rd & Nortz Rd)
  files = list.files(path=paste0('data/simulation/flight_ops/', period), pattern="*.csv", full.names=T, recursive=F)
  
  if (length(files) != 6) {
    stop(paste('Expecting 6 files for period', period, 'and found', length(files)))
  }
  
  # "The number of operations used by the NOISEMAP model is based on the average annual day, per DoD Instruction 4715.13.
  # The average annual day represents the average number of daily airfield operations that would occur during a 24-hour period based
  # on 365 flying days per year; the average annual day is calculated by dividing the total annual airfield operations by 365."
  data = list(
    clean_flight_ops_data(read.csv(files[1])),
    clean_flight_ops_data(read.csv(files[2])),
    clean_flight_ops_data(read.csv(files[3])),
    clean_flight_ops_data(read.csv(files[4])),
    clean_flight_ops_data(read.csv(files[5])),
    clean_flight_ops_data(read.csv(files[6]))
  )
  
  profiles = unique(c(unique(data[[1]]$Profile),
                      unique(data[[2]]$Profile),
                      unique(data[[3]]$Profile),
                      unique(data[[4]]$Profile),
                      unique(data[[5]]$Profile),
                      unique(data[[6]]$Profile)))
  
  # Create a dataframe aggregating all flight operations by taking the maximum operations quantity for each profile
  data_all_ops = as.data.frame(
    bind_rows(data) %>% group_by(Profile) %>% slice(which.max(Num.Total))
  )
  
  # File name for aggregate results
  filename = paste0('data/simulation/_output/csv/NASWI_', period, '_Noisemap - Flight Operations Aggregated.csv')
  
  # Order by profile
  data_all_ops = data_all_ops[order(data_all_ops$Profile),]
  
  # Rename cols for consistency with individual data frames
  colnames(data_all_ops) = names(data[[1]])
  
  # Format columns for xml conversion and BaseOps compatibility
  ops_col_names = c( 
    'Aircraft',
    'Profile',
    'Track',
    'Track Type',
    'Runway',
    'A/C Category',
    'Track Group',
    'Num Day',
    'Num Night',
    'Num Total',
    '% Day',
    '% Night',
    '% Total'
  )
  names(data_all_ops) = ops_col_names
  
  # Remove '% Total' values, as they are incorrect after aggregation
  data_all_ops[,'% Total'] = NA
  
  # Create the aggregate .csv
  write.csv(data_all_ops,
            file=filename,
            row.names=F,
            quote=F,
            na='')
  message(paste('Created', filename))
}

## Total flight operations data, averaging the results of individual period flight operations aggregates into a single table. --------------------------------

files = list.files(path='data/simulation/_output/csv', pattern="*\\Flight Operations Aggregated.csv", full.names=T)
period_ops = lapply(files, read.csv)

# Create a dataframe combining all flight operations periods by taking the average operations quantity for each profile

# Get single row for each unique profile (we will replace values with means below)
total_ops = as.data.frame(
  bind_rows(period_ops) %>% group_by(Profile) %>% slice(which.max(Num.Total))
)

# Calculate mean number of operations per flight profile
numday = as.data.frame(bind_rows(period_ops) %>% group_by(Profile) %>% summarise_at(vars(Num.Day), list(Num.Day = mean)))
numnight = as.data.frame(bind_rows(period_ops) %>% group_by(Profile) %>% summarise_at(vars(Num.Night), list(Num.Night = mean)))
total_mean = merge(numday, numnight, by='Profile', all=T)
total_mean$Num.Total = total_mean$Num.Day + total_mean$Num.Night

# Order by profile
total_ops = total_ops[order(total_ops$Profile),]
total_mean = total_mean[order(total_mean$Profile),]
if (!identical(total_ops$Profile,total_mean$Profile)) {
  stop('Error matching unique profiles')
}

# Replace total_ops Num.X rows with mean calculated ops values
total_ops$Num.Day   = total_mean$Num.Day
total_ops$Num.Night = total_mean$Num.Night
total_ops$Num.Total = total_mean$Num.Total

# Format columns for xml conversion and BaseOps compatibility
names(total_ops) = ops_col_names

# Remove '%' columns, as they are incorrect after aggregation
total_ops = total_ops[, -which(names(total_ops) %in% c('% Day','% Night','% Total'))]

# File name for aggregate results
filename = paste('data/simulation/_output/csv/NASWI_Noisemap - Flight Operations Combined Average.csv')

# Create the aggregate .csv
write.csv(total_ops,
          file=filename,
          row.names=F,
          quote=F,
          na='')
message(paste('Created', filename))

## Nighttime operations only
# File name for nighttime aggregate results
filename_night = paste('data/simulation/_output/csv/NASWI_Noisemap - Flight Operations Combined Average - Night Only.csv')

night_ops = total_ops
night_ops[,'Num Day']   = 0.0
night_ops[,'Num Total'] = total_ops[,'Num Night']

write.csv(night_ops,
          file=filename_night,
          row.names=F,
          quote=F,
          na='')
message(paste('Created', filename_night))

## Simulated increases/reductions in activity

# From BaseOps user manual:
# The operation counts specify the average number of times per calendar day that the flight profile is flown.
# ...
# Annual Ops - the average number of times per year that an airspace profile is flown. The number of monthly operations is calculated by dividing the annual operations by 12. The number of daily operations is calculated by dividing the monthly operations by the average flying days per month.
#
# From Navy EIS: Pattern Operation. An aircraft arrival followed by a departure. Each pattern is considered two operations: the landing or approach is counted as one operation, and the takeoff is counted as another. Pattern operations include the following types:
# - Touch-and-go
# - Field Carrier Landing Practice
# - Ground Controlled Approach / Carrier Controlled Approach

# Pre-2019 expansion:
# "No Action"
# FINAL EIS:
# Total: 84,700
# Ault Field: 78,200
# OLF Coupeville: 6,500
#
# "Action Alternative 2: Scenario A"
# FINAL EIS:
# Total: 112,100
# Ault Field: 88,000
# OLF Coupeville: 24,100

# NOTE: we multiply closed pattern operations by 2, as they are counted as two operations in EIS for comparison
get_tot_n_reported_daily_ops = function(ops) {
  n_closedpattern = sum(ops[ops$`Track Type`=='Closed Pattern', 'Num Total'])
  n_not_closedpattern = sum(ops[ops$`Track Type`!='Closed Pattern', 'Num Total'])
  return(n_closedpattern * 2 + n_not_closedpattern)
}

current_total_annual_ops = get_tot_n_reported_daily_ops(total_ops) * 365
msg('Current simulation total annual operations count:', current_total_annual_ops)

alternative_nops = c( # Alternative number of operations to evaluate
  current_total_annual_ops * 1.5, # 150 % increase
  current_total_annual_ops * 1.25,
  112100, # FINAL EIS 2018: "Action Alternative 2: Scenario A"
  # 84700,   # FINAL EIS 2018: "No Action"
  current_total_annual_ops * 0.75,
  current_total_annual_ops * 0.5 # 50 % decrease
)

multipliers = sapply(alternative_nops, function(o) { o / current_total_annual_ops})

generate_ops_with_multiplier = function(ops, multiplier, nops) {
  tot_ops_m = ops
  tot_ops_m$`Num Day`   = multiplier * tot_ops_m$`Num Day`
  tot_ops_m$`Num Night` = multiplier * tot_ops_m$`Num Night`
  tot_ops_m$`Num Total` = (tot_ops_m$`Num Day` + tot_ops_m$`Num Night`)
  
  filename = glue('data/simulation/_output/csv/alternatives/{multiplier}x ({nops}) - Flight Operations Combined Average.csv')
  write.csv(tot_ops_m, file=filename, row.names=F, quote=F, na='')
  message(paste('Created', filename))
  
  night_ops_m = tot_ops_m
  night_ops_m[,'Num Day']   = 0.0
  night_ops_m[,'Num Total'] = night_ops_m[,'Num Night']
  
  filename = glue('data/simulation/_output/csv/alternatives/{multiplier}x ({nops}) - Flight Operations Combined Average - Night Only.csv')
  write.csv(night_ops_m, file=filename, row.names=F, quote=F, na='')
  message(paste('Created', filename))
}

msg('Generating alternative operations scenarios...')
for (m in 1:length(multipliers)) {
  msg(alternative_nops[m], ' (', multipliers[m], 'x)', sep = '')
  generate_ops_with_multiplier(total_ops, multipliers[m], alternative_nops[m])
}

# Next, see README.md for instructions on how to convert csv to xml and import into BaseOps
