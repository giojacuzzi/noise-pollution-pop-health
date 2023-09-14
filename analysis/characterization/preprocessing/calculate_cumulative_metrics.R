source('metrics/metrics.R')
source('data/load/load_site_date.R')

get_levels_for_org = function(data, org) {
  org = toupper(org)
  return(switch(org,
                'NAVY'=data$LAeq,
                'JGL'=data$LAeq,
                'NPS'=data$LAeq,
                NULL))
}

# Calculate metrics for every site ID and date from an org database (or all, if none is specified), store in a data frame, and save as `data/metrics/metrics.csv`
calculate_site_date_metrics_csv = function(orgarg = '') {

  options(warn = 1)
  file_map = get_file_map()
  if (orgarg != '') {
    orgarg = toupper(orgarg)
    file_map = file_map[file_map$Org==orgarg,]
  }

  metrics = data.frame()
  num_processed = 0
  for (id in unique(file_map$ID)) { # for every site ID
    
    num_processed = num_processed + 1
    name = get_site_name_for_ID(id)
    message(paste0('Processing site ', id, ' \"', name , '\" - ', num_processed, ' of ', length(unique(file_map$ID))))
    
    # readline('Press [enter] to continue...')
    
    for (date in unique(file_map[file_map$ID==id, 'Date'])) { # for every date at that site
      
      # Retrieve the measured sound pressure levels
      site_date_data = load_site_date(id, date)
      org = unique(file_map[file_map$ID==id & file_map$Date==date,]$Org)
      site_date_levels = get_levels_for_org(site_date_data, org)
      if (is.null(site_date_levels)) {
        # TODO: If NAVY, scrape any pre-calculated metrics from the 'Summary' sheet
        warning(paste('Unable to get levels for', id, date, '- skipping...'))
        next
      }
      
      # Determine the total number of measurements
      measurements = nrow(na.omit(site_date_data))
      
      # Calculate the day-night-average metrics from all data measured on that date
      dnl_metrics = LdnFromLevels(site_date_levels, site_date_data$Time)
      Ldn         = round(dnl_metrics$Ldn, digits=2)
      Ldn_Lday    = round(dnl_metrics$Lday, digits=2)
      Ldn_Lnight  = round(dnl_metrics$Lnight, digits=2)
      
      denl_metrics  = LdenFromLevels(site_date_levels, site_date_data$Time)
      Lden          = round(denl_metrics$Lden, digits=2)
      Lden_Lday     = round(denl_metrics$Lday, digits=2)
      Lden_Levening = round(denl_metrics$Levening, digits=2)
      Lden_Lnight   = round(denl_metrics$Lnight, digits=2)
      
      # NOTE: Leqh is identical for Ldn and Lden
      Leqh = round(t(unname(dnl_metrics$Leqh)), digits=2)
      Leq00 = Leqh[1]; Leq01 = Leqh[2]; Leq02 = Leqh[3]; Leq03 = Leqh[4];
      Leq04 = Leqh[5]; Leq05 = Leqh[6]; Leq06 = Leqh[7]; Leq07 = Leqh[8];
      Leq08 = Leqh[9]; Leq09 = Leqh[10]; Leq10 = Leqh[11]; Leq11 = Leqh[12];
      Leq12 = Leqh[13]; Leq13 = Leqh[14]; Leq14 = Leqh[15]; Leq15 = Leqh[16];
      Leq16 = Leqh[17]; Leq17 = Leqh[18]; Leq18 = Leqh[19]; Leq19 = Leqh[20];
      Leq20 = Leqh[21]; Leq21 = Leqh[22]; Leq22 = Leqh[23]; Leq23 = Leqh[24];
      
      # Calculate summary (24hr) metrics for the date
      # NOTE: Summary metrics are intended to represent the entire 24-hour period. As such, we remove any missing data to enable approximate calculations.
      if (anyNA(site_date_data)) {
        site_date_data = na.omit(site_date_data)
        site_date_levels = get_levels_for_org(site_date_data, org)
        warning('Removed NA measurements from summary metrics calculations for the time period. Leq, Lx, and SEL metrics are approximated from available data.')
      }
      
      # NOTE: Primarily A-weighted and time-equalized (`LAeq`) values are used here, but different frequency weightings (C, Z) and time-weightings (slow, fast, impulse) could be used as well
      Leq    = round(LeqTotal(site_date_levels), digits=2) # Leq total (24hr)
      SEL    = ifelse(length(site_date_levels) > 0, round(SelFromLevels(site_date_levels), digits=2), NA)
      if ('Lmax' %in% colnames(site_date_data)) {
        Lmax = ifelse(length(site_date_levels) > 0, max(na.omit(site_date_data$LAFmax)), NA)
      } else {
        Lmax = ifelse(length(site_date_levels) > 0, max(site_date_levels), NA)
      }
      if ('LCpeak' %in% colnames(site_date_data)) {
        LCpeak = ifelse(length(site_date_levels) > 0, max(na.omit(site_date_data$LCpeak)), NA)
      } else {
        LCpeak = NA
      }
      L10    = round(LxFromLevels(site_date_levels, 10),2)
      L25    = round(LxFromLevels(site_date_levels, 25),2)
      L50    = round(LxFromLevels(site_date_levels, 50),2)
      L90    = round(LxFromLevels(site_date_levels, 90),2)
      
      site_date_metrics = data.frame(
        # Metadata
        Org  = org,
        Date = date,
        Name = name,
        ID   = id,
        Measurements = measurements,
        # Ldn
        Ldn,  Ldn_Lday,  Ldn_Lnight,
        # Lden
        Lden, Lden_Lday, Lden_Levening, Lden_Lnight,
        # Leq hourly
        Leq00, Leq01, Leq02, Leq03, Leq04, Leq05, Leq06, Leq07,
        Leq08, Leq09, Leq10, Leq11, Leq12, Leq13, Leq14, Leq15,
        Leq16, Leq17, Leq18, Leq19, Leq20, Leq21, Leq22, Leq23,
        # Summary (24hr)
        Leq, SEL,
        Lmax, LCpeak,
        L10, L25, L50, L90
      )
      
      # Add all metrics for the date to `metrics`
      metrics = rbind(metrics, site_date_metrics)
    }
  }
  
  # Save all metrics data to csv
  file_name = 'data/metrics/_output/'
  if (orgarg == '') {
    file_name = paste0(file_name, 'metrics.csv')
  } else {
    file_name = paste0(file_name, 'metrics_', orgarg, '.csv')
  }
  write.csv(metrics, file=file_name, row.names=F)
  return(metrics)
}
