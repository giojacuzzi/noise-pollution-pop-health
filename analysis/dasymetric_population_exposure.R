source('analysis/spatial_distribution.R')
source('metrics/health_metrics.R')

library(raster)
library(rgdal)
library(glue)
library(mapview)
mapviewOptions(mapview.maxpixels = 50000000)
options(tigris_use_cache = T)

input_path = paste0(here::here(), '/analysis/preprocessing/_output')
output_path = paste0(here::here(), '/analysis/_output')
county_dasypops = list.files(input_path, pattern = paste0('.*dasypop.tif'), full.names = T)

# Read noise contour shapefile layers, including only contours >= health impact thresholds
contours_Ldn = get_contours_Ldn()
contours_Ldn = contours_Ldn[as.numeric(contours_Ldn$Level)>=lden_impact_threshold,]
contours_Lnight = get_contours_Lnight()
contours_Lnight = contours_Lnight[as.numeric(contours_Lnight$Level)>=lnight_impact_threshold,]
contours_Leq24 = get_contours_Leq24()
contours_Leq24 = contours_Leq24[as.numeric(contours_Leq24$Level)>=leq24_impact_threshold,]
mapview(contours_Ldn, zcol='Level') + mapview(contours_Lnight, zcol='Level') + mapview(contours_Leq24, zcol='Level')

# Read individual county dasymetric population rasters
county_rasters = list()
extents = data.frame()
for (c in 1:length(county_dasypops)) {
  county_file = county_dasypops[c]
  message('Reading ', county_file)
  r = raster(county_file)
  county_rasters = append(county_rasters, r)
  e = extent(r)
  extents = rbind(extents, data.frame(
    xmin = e[1], xmax = e[2],
    ymin = e[3], ymax = e[4]
  ))
}
mapview(county_rasters[[3]])

# Combine into a single regional population raster
e = extent(c(
  min(extents$xmin), max(extents$xmax),
  min(extents$ymin), max(extents$ymax)
))
res = res(county_rasters[[1]])
crs = crs(county_rasters[[1]])
county_rasters_p = county_rasters
template = raster(extent(e), res = res, crs = crs)
for (c in 1:length(county_dasypops)) {
  names(county_rasters_p[[c]]) = paste0('County.', gsub('_dasypop', '', names(county_rasters_p[[c]])))
  message('Reprojecting ', names(county_rasters_p[[c]]))
  county_rasters_p[[c]] = projectRaster(county_rasters_p[[c]], template, method = 'ngb')
}

for (c in 1:length(county_dasypops)) {
  message('Merging ', names(county_rasters_p[[c]]))
  if (c == 1) {
    dasy_pop = county_rasters_p[[1]]
  }
  dasy_pop = merge(dasy_pop, county_rasters_p[[c]])
}

# Note: area of Ldn contours is largest, so we use this as our reference extent
# st_area(st_union(st_make_valid(contours_Ldn)))
# st_area(st_union(st_make_valid(contours_Lnight)))
# st_area(st_union(st_make_valid(contours_Leq24)))

# Create cropped population layer
contours_Ldn    = st_transform(contours_Ldn, crs = crs)
contours_Lnight = st_transform(contours_Lnight, crs = crs)
contours_Leq24  = st_transform(contours_Leq24, crs = crs)
dasy_pop_cropped = crop(dasy_pop, as(contours_Ldn, 'Spatial'))
dasy_pop_cropped = mask(dasy_pop_cropped, as(contours_Ldn, 'Spatial'))
dasy_pop_cropped[dasy_pop_cropped == 0] = NA # set all 0 population cells to NA
pop_impacted = projectRaster(dasy_pop_cropped, template, method = 'ngb')
names(pop_impacted) = 'Impacted.Population'
mapview(pop_impacted)

# Create an empty raster with the extent of the contours and the resolution of dasy_pop, and set the projection of the empty raster to match dasy_pop, then rasterize the contour layer using the empty raster
empty_raster = raster(e, res = res)
projection(empty_raster) = projection(template)
contours_Ldn_raster    = rasterize(x = contours_Ldn, y = empty_raster, field = 'Level')
names(contours_Ldn_raster) = 'Ldn'
contours_Lnight_raster = rasterize(contours_Lnight, empty_raster, 'Level')
names(contours_Lnight_raster) = 'Lnight'
contours_Leq24_raster  = rasterize(contours_Leq24, empty_raster, 'Level')
names(contours_Leq24_raster) = 'Leq24'

pop_exposure_stack = stack(append(
  county_rasters_p,
  list(
    pop_impacted,
    contours_Ldn_raster,
    contours_Lnight_raster,
    contours_Leq24_raster
  )
))
mapview(pop_exposure_stack)

# Write layers to file
filename = glue('{output_path}/pop_exposure_stack.grd')
writeRaster(brick(pop_exposure_stack), filename = filename, overwrite = T)
message('Created ', filename)

###############################################################################################
# Insights
# Note that these numbers may be conservative estimates due to the lack of evening-time penalty in DNL calculations, and depending on the exposure-response function used. These numbers also implicitly assume long-term (in some cases yearly average) exposure.
pop_exposure_stack = stack(glue('{output_path}/pop_exposure_stack.grd'))

# Population exposure per 5 dB
exposure_levels_Ldn = data.frame()
for (dB in unique(na.omit(as.vector(pop_exposure_stack[['Ldn']])))) {
  message('Exposure ', dB, ' dB')
  exp_masked = mask(pop_exposure_stack[['Impacted.Population']],
                    clamp(pop_exposure_stack[['Ldn']], lower=dB, upper=dB, useValues=F))
  exposure_levels_Ldn = rbind(exposure_levels_Ldn, data.frame(
    Level=dB,
    Population=cellStats(exp_masked, 'sum')
  ))
}
ggplot(exposure_levels_Ldn, aes(x=Level, y=Population)) + geom_bar(position='dodge', stat='identity') + ggtitle('Estimated population exposed per 5dB Ldn') + xlab('Ldn (dB)') + ylab('Estimated population') + geom_text(aes(label=round(Population)), position=position_dodge(width=0.9), vjust=-0.25)

# Total area of noise exposure above an ~ARBITRARY~ "audibility" threshold
audibility_threshold = 35
contours_audible_Leq24 = get_contours_Leq24()
contours_audible_Leq24 = contours_audible_Leq24[as.numeric(contours_audible_Leq24$Level)>=audibility_threshold,]
audibility_area = st_area(st_make_valid(st_union(contours_audible_Leq24)))
units::set_units(audibility_area, km^2)
units::set_units(audibility_area, mi^2)

# Total area of noise exposure associated with adverse health effects (note this includes water)
contours_Ldn = get_contours_Ldn()
impact_area = st_area(st_make_valid(st_union(contours_Ldn[contours_Ldn$Level>=lden_impact_threshold,])))
units::set_units(impact_area, km^2)
units::set_units(impact_area, mi^2)
