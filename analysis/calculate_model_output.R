# Site period values taken from Real-Time Modeled .poi ("TOTAL") outputs for a given site, all monitoring periods
site_period = c(72.6, 63.4, 65.9, 53.5)

# This should match the results listed in Table 11, "Real-Time Modeled DNL", page 54 of the technical report.
site_dnl = round(10*log10(sum(10^(site_period/10))/4), 1) # energy average
