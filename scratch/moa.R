source('data/load/load_file_navy.R')

data_file = load_file_navy(
  # path = '/Users/giojacuzzi/../../Volumes/gioj/PHI/NAVY/Acoustic Data/Data/HOH/NASWI_Site_99_HOH_2021_08_B/DoD SAFE-yBEip2E44fq35aHo/NASWI_Site_99_HOH_2021_08/831C_11165-20210818 000000-21081800.LD0.xlsx',
  path = '/Users/giojacuzzi/../../Volumes/gioj/PHI/NAVY/Acoustic Data/Data/HOH/NASWI_Site_99_HOH_2021_04_B/DoD SAFE-thxuRqA4FwhNXV5z/NASWI_Site_99_HOH_2021_04/831C_11165-20210429 000000-21042900.LD0.xlsx',
  colnames = c(
    'Time',
    # A-weighted
    'LAeq','LApk',
    'LAS','LASmax',
    'LAF','LAFmax',
    'LAI','LAImax',
    # C-weighted
    'LCeq','LCpk',
    'LCS','LCSmax',
    'LCF','LCFmax',
    'LCI','LCImax',
    # Z-weighted
    'LZeq','LZpk',
    'LZS','LZSmax',
    'LZF','LZFmax',
    'LZI','LZImax',
    # Frequency content
    '1/3 LZeq 6.3',
    '1/3 LZeq 8.0',
    '1/3 LZeq 10.0',
    '1/3 LZeq 12.5',
    '1/3 LZeq 16.0',
    '1/3 LZeq 20.0',
    '1/3 LZeq 25.0',
    '1/3 LZeq 31.5',
    '1/3 LZeq 40.0',
    '1/3 LZeq 50.0',
    '1/3 LZeq 63.0',
    '1/3 LZeq 80.0',
    '1/3 LZeq 100',
    '1/3 LZeq 125',
    '1/3 LZeq 160',
    '1/3 LZeq 200',
    '1/3 LZeq 250',
    '1/3 LZeq 315',
    '1/3 LZeq 400',
    '1/3 LZeq 500',
    '1/3 LZeq 630',
    '1/3 LZeq 800',
    '1/3 LZeq 1000',
    '1/3 LZeq 1250',
    '1/3 LZeq 1600',
    '1/3 LZeq 2000',
    '1/3 LZeq 2500',
    '1/3 LZeq 3150',
    '1/3 LZeq 4000',
    '1/3 LZeq 5000',
    '1/3 LZeq 6300',
    '1/3 LZeq 8000',
    '1/3 LZeq 10000',
    '1/3 LZeq 12500',
    '1/3 LZeq 16000',
    '1/3 LZeq 20000'
  ))

source('analysis/noise_regime/preprocessing/calculate_single_event_metrics.R')

id = 'HOH'
org = 'NAVY'
data = data_file[[1]]
# STEP THROUGH find_events_for_site_date starting from line 100