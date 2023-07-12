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


# Evaluate spatial distribution (noise contours)
source('analysis/spatial_distribution.R')
source('analysis/population_exposure.R')
