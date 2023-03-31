## Cognitive development in children and speech intelligibility

source('global.R')
source('data/metrics/metrics.R')
library(mapview)
library(leafem)

data_sites   = get_data_sites()
data_metrics = get_data_metrics()

## Cognitive development in children -------------------------------------------

# WHO guidelines: Reading and oral comprehension 1–2-month delay per 5 dB (outdoor Lden) increase. Relevant RR increase of impaired reading and oral comprehension at 55 dB (1 month delay in reading and oral comprehension, as assessed by standardized tests)
# https://nap.nationalacademies.org/catalog/22433/assessing-aircraft-noise-conditions-affecting-student-learning-volume-1-final-report

data_metrics[data_metrics$Lden>=55,]

## Speech intelligibility / interference ---------------------------------------

# ANSI 12.3 states that the criteria for allowable background noise level can be relaxed for irregular noise events. This is because speech is impaired only for the short time when the aircraft noise is close to its maximum. Consequently, when the background noise level of the noisiest hour is dominated by aircraft noise, the indoor criteria can be increased. The Leq of 35 dB for continuous background noise can be increased by 5 dB to an Leq of 40 dB. However, the noise level cannot exceed 40 dB for more than 10% of the noisiest hour. This is for a room that's less than 20,000 cubic feet.

# SEL has been recommended by some as a better choice for estimating speech interference from aircraft overflights indoors. A maximum SEL of 64 dB is suggested. A 26 dB noise reduction is assumed when you move indoors from outdoors. So, a 64 dB SEL indoors is about equal to 90 dB SEL outdoors. Aircraft events with outdoor SEL values greater than 90 dB would disrupt indoor speech communication. The research indicates that speakers using a casual vocal effort can achieve 95% intelligibility when indoor SEL values did not exceed 60 dB. This translates to an approximately 50 dB Lmax.