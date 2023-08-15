#  Total flight operations data, aggregating the results of individual period flight operations aggregates from load_flight_operations.R into a single table representing the maximum extent of exposure across all periods combined.

# files = list.files(path='data/flight_ops/modeling/_output/', pattern="*Flight Operations.csv", full.names=T)
files = list.files(path='data/flight_ops/modeling/_output/', pattern=".*\\Period.*\\Flight Operations.csv", full.names=T)
period_ops = lapply(files, read.csv)

# Create a dataframe aggregating all flight operations periods by taking the maximum operations quantity for each profile
library(dplyr)

# get single row for each unique operation, we will replace max values with means below
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
  message('Error matching unique profiles')
  stop()
}

# Replace total_ops Num.X rows with mean calculated ops values
total_ops$Num.Day   = total_mean$Num.Day
total_ops$Num.Night = total_mean$Num.Night
total_ops$Num.Total = total_mean$Num.Total


# File name for aggregate results
filename = paste('data/flight_ops/modeling/_output/Total Aggregate Flight Operations.csv')

# Format columns for xml conversion and BaseOps compatibility
names(total_ops) = c( 
  'Aircraft',
  'Engine',
  'Profile',
  'Long Name',
  'Track',
  'Track Type',
  'Runway',
  'Noise Model',
  'A/C Category',
  'Rep',
  'Track Group',
  'Num Day',
  'Num Night',
  'Num Total',
  '% Day',
  '% Night',
  '% Total'
)

# Remove '%' values, as they are incorrect after aggregation
total_ops[,'% Day'] = NA
total_ops[,'% Night'] = NA
total_ops[,'% Total'] = NA

# Create the aggregate .csv
write.csv(total_ops,
          file=filename,
          row.names=F,
          quote=F,
          na='')
message(paste('Created', filename))

# Next, see README.md for instructions on how to convert csvs to xml and import into BaseOps

## Nighttime ops only ----------------------------------------------------------
# File name for nighttime aggregate results
filename_night = paste('data/simulation/_output/Total Aggregate Flight Operations - NIGHT.csv')

night_ops = total_ops
night_ops[,'Num Day']   = 0.0
night_ops[,'Num Total'] = total_ops[,'Num Night']

write.csv(night_ops,
          file=filename_night,
          row.names=F,
          quote=F,
          na='')
message(paste('Created', filename_night))
