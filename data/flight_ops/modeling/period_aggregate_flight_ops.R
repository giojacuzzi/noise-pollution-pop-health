# Aggregated flight operations data per-period

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

# Unique active operations (i.e. 'profiles' that have non-zero operations values)
get_num_active_ops = function(data) {
  return(length(unique(data[data$Num.Total>0,'Profile'])))
}

get_total_quantity_ops = function(data) {
  return(sum(data$Num.Total))
}

periods = c('MP1','MP2','MP3','MP4')

overview = data.frame()
for (period in periods) {
  
  # Get all six files for the period...
  # NASWI_MP1_Noisemap - Coupeville, includes points-of-interest:
  #   20B_SG, 24A_B, 25B_T, 26B_SG, 27A_SG, and 33_SG
  # NASWI_MP1_02_Noisemap - 2B_T Seaplane Base
  # NASWI_MP1_03_Noisemap - 3A_T Skagit River Dike (WDFW Site)
  # NASWI_MP1_05_Noisemap - 5B_SG Lopez Island (Point Colville)
  # NASWI_MP1_08_Noisemap - 8B_SG Dog Park (North Whidbey Parks & Rec)
  # NASWI_MP1_09_Noisemap - 9B_SG NASWI Gate (Corner of Banta Rd & Nortz Rd)
  files = list.files(path=paste0('data/flight_ops/modeling/data/original_exports/', period), pattern="*.csv", full.names=T, recursive=F)
  
  if (length(files) != 6) {
    warning(paste('Expecting 6 files for period', period, 'and found', length(files)))
    next
  }
  
  # "The number of operations used by the NOISEMAP model is based on the average annual day, per DoD Instruction 4715.13. The average annual day represents the average number of daily airfield operations that would occur during a 24-hour period based on 365 flying days per year; the average annual day is calculated by dividing the total annual airfield operations by 365."
  data = list(
    clean_flight_ops_data(read.csv(files[1])),
    clean_flight_ops_data(read.csv(files[2])),
    clean_flight_ops_data(read.csv(files[3])),
    clean_flight_ops_data(read.csv(files[4])),
    clean_flight_ops_data(read.csv(files[5])),
    clean_flight_ops_data(read.csv(files[6]))
  )
  
  num_active_ops = lapply(data, get_num_active_ops)
  total_quantity_ops = lapply(data, get_total_quantity_ops)
  
  profiles = unique(c(unique(data[[1]]$Profile),
                      unique(data[[2]]$Profile),
                      unique(data[[3]]$Profile),
                      unique(data[[4]]$Profile),
                      unique(data[[5]]$Profile),
                      unique(data[[6]]$Profile)))
  
  # Create a dataframe aggregating all flight operations by taking the maximum operations quantity for each profile
  library(dplyr)
  data_all_ops = as.data.frame(
    bind_rows(data) %>% group_by(Profile) %>% slice(which.max(Num.Total))
  )
  
  orgdata = bind_rows(data)
  bind_rows(data) %>% group_by(Profile) %>% summarise_at(vars(Num.Total), list(name = max))
  bind_rows(data) %>% group_by(Profile) %>% summarise_at(vars(Num.Total), list(name = mean))
  
  # File name for aggregate results
  filename = paste('data/flight_ops/_output/Period', period, 'Aggregate Flight Operations.csv')
  
  # Order by profile
  data_all_ops = data_all_ops[order(data_all_ops$Profile),]
  
  # Rename cols for consistency with individual data frames
  colnames(data_all_ops) = names(data[[1]])

  # Add to total overview file
  aggregate_num_active_op_profiles = get_num_active_ops(data_all_ops)
  aggregate_total_quantity_ops = get_total_quantity_ops(data_all_ops)
  overview = rbind(
    overview,
    data.frame(
      Period           = period,
      File             = c(basename(files), basename(filename)),
      ActiveOpProfiles = c(unlist(num_active_ops), aggregate_num_active_op_profiles),
      TotalQuantityOps = c(unlist(total_quantity_ops), aggregate_total_quantity_ops)
    )
  )
  
  # Format columns for xml conversion and BaseOps compatibility
  names(data_all_ops) = c( 
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

# Save total overview file
overview_filename = paste0('data/flight_ops/_output/Overview.csv')
write.csv(overview,
          file=overview_filename,
          row.names=F,
          quote=F,
          na='')
message(paste('Created', overview_filename))

# Next, see README.md for instructions on how to convert csv to xml and import into BaseOps
