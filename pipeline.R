### The entire data processing pipeline. Requirements are the PHI database and `data/sites.csv`

# 1. Create tables mapping org files to sites and dates
source('data/load_file_navy.R')
file_map_navy = map_files_navy_csv() # produces `data/files_navy.csv`
source('data/load_file_sda.R')
file_map_sda = map_files_sda_csv()  # produces `data/files_sda.csv`
source('data/load_file_nps.R')
file_map_nps = map_files_nps_csv()  # produces `data/files_sda.csv`

file_map = get_file_map() # requires both .csv files

# 2. Calculate metrics for each site date
source('analysis/calculate_metrics.R')
calculate_metrics_csv() # produces `data/metrics/metrics.csv`

# 3. Plot and map results
source('results.R')
