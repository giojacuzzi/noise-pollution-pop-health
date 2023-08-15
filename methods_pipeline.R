### The entire data processing methods pipeline.
# Requirements are the PHI database and 'data/sites.csv'

# NOTE: Configure 'database_path' global variable
# to your local path to the PHI database
source('global.R')

# 0. Retrieve site location information ----------------
sites = st_as_sf(get_data_sites(),
                 coords = c('Longitude', 'Latitude'), 
                 crs = crs, agr = 'constant')
sites = na.omit(sites)
sites = sites[sites$ID %in% unique(get_data_metrics()[,'ID']), ]
sites$Longitude = st_coordinates(sites$geometry)[,'X']
sites$Latitude  = st_coordinates(sites$geometry)[,'Y']
mapview(sites)

# 1. Create tables mapping org files to sites and dates ------------------------
# input: PHI database, 'data/sites.csv'
# outputs: 'data/load/_output/file_map_[ORG].csv'
source('data/load/load_file_jgl.R')
file_map_jgl = map_files_jgl_csv()
source('data/load/load_file_navy.R')
file_map_navy = map_files_navy_csv()
source('data/load/load_file_sda.R')
file_map_sda = map_files_sda_csv()
source('data/load/load_file_nps.R')
file_map_nps = map_files_nps_csv()
file_map = get_file_map() # requires all file_map .csv files

# 2. Load SPL data for each site date ------------------------------------------
# inputs: PHI database, 'file_map_[ORG].csv'
# outputs: '[database_path]/converted/site_dates/[ORG]/[ID]_[DATE].csv'
source('data/load/load_site_date.R')
create_site_date_csvs('JGL')
create_site_date_csvs('NAVY')
create_site_date_csvs('SDA')
create_site_date_csvs('NPS')

# 3. Find noise events for each site date --------------------------------------
# inputs: '[database_path]/converted/site_dates/[ORG]/[ID]_[DATE].csv'
# outputs: 'data/events/_output/events_[ORG].csv'
source('data/analysis/characterization/preprocessing/evaluate_events.R')
calculate_events_csv('JGL')
calculate_events_csv('NAVY')
calculate_events_csv('SDA')
calculate_events_csv('NPS')
# 3.5. Get noise events reported by Navy
# inputs: PHI database
# outputs: 'data/events/_output/navy_reported_events.csv'
source('data/events/load_events_navy.R')

# 4. Calculate metrics for each site date --------------------------------------
# source('analysis/calculate_metrics.R')
# Produce 'data/metrics/metrics.csv'
# calculate_site_date_metrics_csv()

# 6. Calculate OSHA/NIOSH TWA for each site date -------------------------------

#.....

####################################
# SIMULATION

## Prepare data for noise simulation model ----------------------------

# Download "V. Noise Modeling Data - PUBLIC_NOISEMAP" from NAVY database and unzip
#
# https://www.navfac.navy.mil/Portals/68/Documents/Business-Lines/Asset-Management/Sound/Remaining-Adds/PUBLIC_NOISEMAP.zip?ver=KEbUPIKWwvnjZl0H4vUg9g%3d%3d
# https://www.navfac.navy.mil/Portals/68/Documents/Business-Lines/Asset-Management/Sound/Read-Me-Files/PUBLIC_NOISEMAP_Files_README.txt?ver=AOs5DYQ2sccT1gAfoxO7Ow%3d%3d

# Format flight operations
# Input: NOISEMAP/NASWI/MP[N]/*Flight Operations.xml, where 'N' is the monitoring period, 1-4
# Output: .csv for each Flight Operations.xml file, located under 'data/simulation/flight_ops'
# 
# For each Flight Operations.xml file, open in Microsoft Excel. If prompted, do not update workbook links. Then, select File > Save As > Comma Separated Values (.csv), and save to the corresponding directory in 'data/simulation/flight_ops/MP[N]'

# Run 'data/simulation/flight_ops/aggregate_flight_ops.R' to generate the per-period aggregates and the total combined average .csv outputs, respectively.
# Input: 'data/simulation/flight_ops/*.csv'
# Outputs:
# 'data/simulation/flight_ops/_output/NASWI_MP[N]_Noisemap - Flight Operations Aggregated.csv'
# 'data/simulation/flight_ops/_output/NASWI_Noisemap - Flight Operations Combined Average.csv'
# 'data/simulation/flight_ops/_output/NASWI_Noisemap - Flight Operations Combined Average - Night Only.csv'
source('data/simulation/aggregate_flight_ops.R')

# Next, open the 'data/simulation/_output/csv/* Combined Average *.csv' files with Microsoft Excel and re-save as .xml files in data/simulation/_output/xml.
# NOTE: We currently only have day/night values (not evening) for the operations on average annual day, so in the next step,
# we cannot import any data from an 'evening' column.

# Open 'simulation/DNL/NASWI_Combined_Average_DNL.baseops' with BaseOps
# To import new flight operations into BaseOps:
# - File > Import Flight Operations from Spreadsheet
#   - Option Categories > File > Import operations from the following spreadsheet file: <the xml file you just saved, either the total combined average for DNL and LEQ24, or the "Night Only" for LNIGHT>
#   - Option Categories > Columns:
#     - Flight Profile Name Column: B (2)
#     - Num Day Ops Column: H (8)
#     - Num Night Ops Column: I (9)
#   - Also import flight tracks: yes
#     - Flight Track Name Column: C (3)
#   - Option Categories > Missing Data
#     - If a flight profile in the spreadsheet is missing from the BaseOps case, then… Add the missing profile to the BaseOps case
#     - If a flight profile in the BaseOps case is missing from the spreadsheet, then… Leave the profile unchanged in the BaseOps case
#     - If you are also importing flight tracks, and a flight track in the spreadsheet is missing from the BaseOps case, then… Set the profile's flight track to "undefined"
# - Press OK… You should see the following message:
#  Importing flight profiles from spreadsheet file NASWI_MP1_Noisemap - Flight Operations.xml
#  The following flight profiles appear in both the BaseOps case and the spreadsheet file.
#  The daily flight profile operation counts in the BaseOps case will be updated to match the values in the spreadsheet file.

# Generate noise contour maps
# See NoiseMap and AEDT Gap Analysis Technical Report for further details of NoiseMap and BaseOps software. NoiseMap BaseOps cases have been made for both DNL (cumulative, see 4.1.1.1) and Lnight (9hr, see 4.1.1.2).
#
# TODO

#.....


## Run noise simulation model ----------------------------
# Open '/Users/giojacuzzi/repos/PHI-Noise-Pollution/data/flight_ops/modeling/baseops/Aggregated/DNL/NASWI_Aggregated_Noisemap.baseops' with BaseOps
# Run the case, then select Case > Plot to open NMPlot
# Edit Options > Countours > Levels > Manually specify contour levels from lowest 10 to highest 150, spacing between primary levels 1, and number of secondary levels 0 > Apply
# File > Export to GIS > ESRI ARC/INFO Shpaefile (SHP)








