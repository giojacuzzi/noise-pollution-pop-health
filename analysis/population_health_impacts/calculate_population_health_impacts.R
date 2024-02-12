## Calculate predicted population health impacts from noise exposure

library(raster)
library(sf)
library(glue)
library(gdalUtils)
library(rslurm)
library(fasterize)
library(mapview)
mapviewOptions(mapview.maxpixels = 50000000)
options(tigris_use_cache = T)

source('global.R')
source('simulation/contours.R')

source('metrics/metrics.R')
source('metrics/thresholds.R')
source('metrics/exposure_response_functions.R')

input_path = paste0(here::here(), '/analysis/population_noise_exposure/_output/noise_exposure')
pop_exposure_stack = stack(glue('{input_path}/pop_exposure_stack.grd'))

msg('Starting population health impact calculation...')

# Calculate rasters for HA, HSD, HL/CE
msg('Calculating raster for HA (FICON)...')
percent_HA_FICON = calc(pop_exposure_stack[['Ldn']],
                        fun=function(L) { exp_resp_bounded(exp_resp_FICON, L, bounds_FICON) })
msg('Calculating raster for HA (ISO)...')
percent_HA_ISO = calc(pop_exposure_stack[['Ldn']],
                      fun=function(L) { exp_resp_bounded(exp_resp_ISO_Miedema_Ldn, L, bounds_iso_miedema) })
msg('Calculating raster for HA (WHO)...')
percent_HA_WHO = calc(pop_exposure_stack[['Ldn']],
                      fun=function(L) { exp_resp_bounded(exp_resp_WHO, L, bounds_who) })
msg('Calculating raster for HA (FAA NES)...')
percent_HA_FAANES = calc(pop_exposure_stack[['Ldn']],
                         fun=function(L) { exp_resp_bounded(exp_resp_FAANES, L, bounds_FAANES) })
msg('Calculating raster for HA (Yokoshima)...')
percent_HA_Yokoshima = calc(pop_exposure_stack[['Ldn']],
                            fun=function(L) { exp_resp_bounded(exp_resp_Yokoshima, L, bounds_Yokoshima) })
msg('Calculating raster for HSD (WHO)...')
percent_HSD_WHO = calc(pop_exposure_stack[['Lnight']],
                       fun=function(L) { exp_resp_bounded(exp_resp_HSD_WHO, L, bounds_HSD) })
msg('Calculating raster for HSD (Smith)...')
percent_HSD_Smith = calc(pop_exposure_stack[['Lnight']],
                         fun=function(L) { exp_resp_bounded(exp_resp_HSD_Smith, L, bounds_HSD) })
