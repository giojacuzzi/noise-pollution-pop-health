## Hearing loss and OSHA / NIOSH standards

source('global.R')
source('data/metrics/metrics.R')
library(mapview)
library(leafem)

data_sites   = get_data_sites()
data_metrics = get_data_metrics()

# OSHA/NIOSH violation ---------------------------------------------------------
# TODO: calculate OSHA/NIOSH with data/metrics/calculate_osha_niosh.R

# Hearing loss -----------------------------------------------------------------
# Dependencies: any dataset

occupational_standards = read.csv('data/metrics/output/osha_niosh.csv')

# OSHA action level
# https://www.osha.gov/laws-regs/regulations/standardnumber/1910/1910.95AppA#:~:text=(2)%20The%20eight%2Dhour,to%20the%20measured%20sound%20level
unique(occupational_standards[occupational_standards$OshaTWA>=85,'ID'])

# NIOSH recommended exposure limit
# https://www.cdc.gov/niosh/docs/98-126/pdfs/98-126.pdf?id=10.26616/NIOSHPUB98126
unique(occupational_standards[occupational_standards$NioshTWA>=85,'ID'])

# FAA Hearing Conservation Program action level trigger
# https://www.faa.gov/documentLibrary/media/Order/Order_3900.66A.pdf
# "The AL or the TWA exposure which requires program inclusion is 82 dBA, or a dose of 50 percent. FS employees exposed to this level for 30 days or more per year require inclusion in the HCP."
unique(occupational_standards[occupational_standards$NioshTWA>=82,'ID'])

# According to ISO 1999, exposure to environmental and leisure-time noise with LAeq,24h values < 70 dB(A) does not cause hearing impairment in the large majority of people (> 95%). In other words, an exposure limit of >70 dBA LAeq over a 24 hour period from environmental and leisure noise can pose a risk of hearing impairment.
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1637786/pdf/envhper00310-0128.pdf
# The EPA identifies a 24-hour exposure level of 70 decibels as the level of environmental noise which will prevent any measurable hearing loss over a lifetime.
# https://www.epa.gov/archive/epa/aboutepa/epa-identifies-noise-levels-affecting-health-and-welfare.html#:~:text=The%20document%20identifies%20a%2024,hearing%20loss%20over%20a%20lifetime
unique(data_metrics[data_metrics$Leq>=70,'ID'])

# Direct physiological effects related to potential health impairments and risks are found, as a rule, only where maximal sound levels are above ca 115 d B(A) Criteria for the risk of inner ear damage due to high peak noise levels (> 115 d B)
# https://drive.google.com/drive/u/0/search?q=Low-altitude%20overflights%20of%20fighters%20the%20risk%20of%20hearing%20loss
# Noise-induced hearing loss is generally from exposures to higher noise frequencies ranging from 3,000 to 6,000 Hz
# https://drive.google.com/file/d/1WZX8iRGYmG4wTG41XTzZzUyjHAkpPr2L/view
unique(data_metrics[data_metrics$Lmax>=115,'ID'])

# TODO: threshold of pain (typically 120-140)