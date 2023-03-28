### The entire data processing pipeline. Requirements are the PHI database and `data/sites.csv`

# NOTE: Configure 'database_path' global variable to your local path to the PHI database
source('global.R')

# 1. Create tables mapping org files to sites and dates
# Produce 'data/load/output/file_map_[ORG].csv'
source('data/load/load_file_jgl.R')
file_map_jgl = map_files_jgl_csv()
source('data/load/load_file_navy.R')
file_map_navy = map_files_navy_csv()
source('data/load/load_file_sda.R')
file_map_sda = map_files_sda_csv()
source('data/load/load_file_nps.R')
file_map_nps = map_files_nps_csv()

file_map = get_file_map() # requires all .csv files

# 2. Load SPL data for each site date
source('data/load/load_site_date.R')
# Produce '[database_path]/converted/site_dates/[ORG]/[ID]_[DATE].csv'
create_site_date_csvs('JGL')
create_site_date_csvs('NAVY')
create_site_date_csvs('SDA')
create_site_date_csvs('NPS')

# 3. Find noise events for each site date
source('data/events/evaluate_events.R')
# Produce 'data/events/output/events_[ORG].csv'
calculate_events_csv('JGL')
calculate_events_csv('NAVY')
calculate_events_csv('SDA')
calculate_events_csv('NPS')
# Produce 'data/events/output/navy_reported_events.csv'
source('data/events/load_events_navy.R')

# 4. Validate noise events for each site date, ensuring they are aircraft events

# # 5. Calculate metrics for each site date
# source('analysis/calculate_metrics.R')
# Produce 'data/metrics/metrics.csv'
# calculate_site_date_metrics_csv()

# 6. Calculate OSHA/NIOSH TWA for each site date


