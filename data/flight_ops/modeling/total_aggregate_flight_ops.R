#  Total flight operations data, aggregating the results of individual period flight operations aggregates from load_flight_operations.R into a single table representing the maximum extent of exposure across all periods combined.

files = list.files(path='data/flight_ops/output', pattern="*Flight Operations.csv", full.names=T)
period_ops = lapply(files, read.csv)

# Create a dataframe aggregating all flight operations periods by taking the maximum operations quantity for each profile
library(dplyr)
total_ops = as.data.frame(
  bind_rows(period_ops) %>% group_by(Profile) %>% slice(which.max(Num.Total))
)

# Order by profile
total_ops = total_ops[order(total_ops$Profile),]

# File name for aggregate results
filename = paste('data/flight_ops/output/Total Aggregate Flight Operations.csv')

# TODO: Overview stats

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

# Remove '% Total' values, as they may be incorrect after aggregation
total_ops[,'% Total'] = NA

# Create the aggregate .csv
write.csv(total_ops,
          file=filename,
          row.names=F,
          quote=F,
          na='')
message(paste('Created', filename))

# Next, see README.md for instructions on how to convert csv to xml and import into BaseOps