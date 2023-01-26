# NOTE: The 'NASWI_MPX_Y_Noisemap - Flight Operations.xml' files have been exported from the corresponding .baseops case via BaseOps, where X is the monitoring period and Y is the file number (for Ault Field; Coupeville doesn't have this). These exports have been copied over to 'data/Noise Modeling Data/Exports/MP1', then opened in MS Excel, and saved as the .csv files used here.

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

files = c(
  # Coupeville includes points-of-interest 20B_SG, 24A_B, 25B_T, 26B_SG, 27A_SG, and 33_SG
  'data/Noise Modeling Data/Exports/MP1/NASWI_MP1_Noisemap - Flight Operations.csv',
  # 2B_T Seaplane Base
  'data/Noise Modeling Data/Exports/MP1/NASWI_MP1_02_Noisemap - Flight Operations.csv',
  # 3A_T Skagit River Dike (WDFW Site)
  'data/Noise Modeling Data/Exports/MP1/NASWI_MP1_03_Noisemap - Flight Operations.csv',
  # 5B_SG Lopez Island (Point Colville)
  'data/Noise Modeling Data/Exports/MP1/NASWI_MP1_05_Noisemap - Flight Operations.csv',
  # 8B_SG Dog Park (North Whidbey Parks & Rec)
  'data/Noise Modeling Data/Exports/MP1/NASWI_MP1_08_Noisemap - Flight Operations.csv',
  # 9B_SG NASWI Gate (Corner of Banta Rd & Nortz Rd)
  'data/Noise Modeling Data/Exports/MP1/NASWI_MP1_09_Noisemap - Flight Operations.csv'
)

# "The number of operations used by the NOISEMAP model is based on the average annual day, per DoD Instruction 4715.13. The average annual day represents the average number of daily airfield operations that would occur during a 24-hour period based on 365 flying days per year; the average annual day is calculated by dividing the total annual airfield operations by 365."
data = list(
  clean_flight_ops_data(read.csv(files[1])),  # 157 unique active operations (i.e. 'profiles')
  clean_flight_ops_data(read.csv(files[2])),  # 101
  clean_flight_ops_data(read.csv(files[3])),  # 106
  clean_flight_ops_data(read.csv(files[4])),  # 67
  clean_flight_ops_data(read.csv(files[5])),  # 119
  clean_flight_ops_data(read.csv(files[6]))   # 131
)

profiles = unique(c(unique(data[[1]]$Profile),
                    unique(data[[2]]$Profile),
                    unique(data[[3]]$Profile),
                    unique(data[[4]]$Profile),
                    unique(data[[5]]$Profile),
                    unique(data[[6]]$Profile)))

# Create a dataframe of all flight operations by taking the maximum operations quantity for each profile
data_all_ops = data.frame()
for (profile in profiles) {
  ops_coupeville = data[[1]][data[[1]]$Profile==profile, 'Num.Total']
  ops_2B_T       = data[[2]][data[[2]]$Profile==profile, 'Num.Total']
  ops_3A_T       = data[[3]][data[[3]]$Profile==profile, 'Num.Total']
  ops_5B_SG      = data[[4]][data[[4]]$Profile==profile, 'Num.Total']
  ops_8B_SG      = data[[5]][data[[5]]$Profile==profile, 'Num.Total']
  ops_9B_SG      = data[[6]][data[[6]]$Profile==profile, 'Num.Total']
  ops = c(ops_coupeville, ops_2B_T, ops_3A_T, ops_5B_SG, ops_8B_SG, ops_9B_SG)
  
  maxops = max(ops)
  idx = which(ops==maxops)[1]
  # print(paste(profile, ':', toString(ops), ' - max', maxops, 'idx', idx))
  data_max = data[[idx]]
  data_all_ops = rbind(data_all_ops, data_max[data_max$Profile==profile,])
}

# Format for xml conversion and save
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

# Using Microsoft Excel, create a spreadsheet containing the following:
# - Flight profile name
# - Flight track name
# - Number of day operations
# - Number of evening operations 
# - Number of night operations
# NOTE: We currently only have day/night values for the operations on average annual day

write.csv(data_all_ops,
          file='data/Noise Modeling Data/Exports/MP1/NASWI - All Flight Operations.csv',
          row.names=F)

# Next, open the csv with Excel, and re-save it as an xml

# In baseops:
# File > Import Flight Operations from Spreadsheet
# Option Categories > File > Import operations from the following spreadsheet file: <the xml file you just saved>
# Option Categories > Columns:
#   Flight Profile Name Column: C (3)
#   Num Day Ops Column: L (12)
#   Num Night Ops Column: M (13)
#   Also import flight tracks: yes
#   Flight Track Name Column: E (5)
# Option Categories > Missing Data
#   If a flight profile in the spreadsheet is missing from the BaseOps case, then... Add the missing profile to the BaseOps case
#   If a flight profile in the BaseOps case is missing from the spreadsheet, then... Leave the profile unchanged in the BaseOps case
#   If you are also importing flight tracks, and a flight track in the spreadsheet is missing from the BaseOps case, then... Set the profile's flight track to "undefined"
# Press OK... You should see the following message:
#   Importing flight profiles from spreadsheet file NASWI_MP1_Noisemap - Flight Operations.xml  
#   The following flight profiles appear in both the BaseOps case and the  
#   spreadsheet file.  
# 
#   The daily flight profile operation counts in the BaseOps case will be updated to  
#   match the values in the spreadsheet file.  
# 
#   226A_EXP  
#   226A_FLT  
#   226A_FRS
#   ...
# Press OK again
