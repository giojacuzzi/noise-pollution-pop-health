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

# Next, see README.md for instructions on how to convert csv to xml and import into BaseOps