### The entire data processing pipeline. Requirements are the PHI database and `data/sites.csv`

# NOTE: Configure 'database_path' global variable to your local path to the PHI database
source('global.R')

# 1. Create tables mapping org files to sites and dates
# Produce data/load/output/file_map_[ORG].csv
source('data/load/load_file_jgl.R')
file_map_jgl = map_files_jgl_csv()
source('data/load/load_file_navy.R')
file_map_navy = map_files_navy_csv()
source('data/load/load_file_sda.R')
file_map_sda = map_files_sda_csv()
source('data/load/load_file_nps.R')
file_map_nps = map_files_nps_csv()

file_map = get_file_map() # requires all .csv files

# 2. Calculate metrics for each site date
source('analysis/calculate_metrics.R')
calculate_site_date_metrics_csv() # produces `data/metrics/metrics.csv`

# 3. Plot and map results
source('results.R')
