# Input: dasypop .tif files
# Output: analysis/preprocessing/_output/pop_exposure_stack .gri and .grd

source('simulation/contours.R')
source('metrics/health_metrics.R')

library(raster)
library(rgdal)
library(glue)
library(mapview)
mapviewOptions(mapview.maxpixels = 50000000)
options(tigris_use_cache = T)

generate_pop_exposure_stack = function() {
  input_path = paste0(here::here(), '/analysis/preprocessing/_output')
  output_path = paste0(here::here(), '/analysis/preprocessing/_output')
  dasypops = list.files(input_path, pattern = paste0('dasypop.*.tif'), full.names = T)
  
  # Read noise contour shapefile layers, including only contours >= health impact thresholds
  msg('Reading noise contours...')
  contours_Ldn = get_contours_Ldn(threshold = lden_impact_threshold)
  contours_Lnight = get_contours_Lnight(threshold = lnight_impact_threshold)
  contours_Leq24 = get_contours_Leq24(threshold = HL_leq24_impact_threshold)
  # mapview(contours_Ldn, zcol='Level') + mapview(contours_Lnight, zcol='Level') + mapview(contours_Leq24, zcol='Level')
  
  # Read individual county / native land dasymetric population rasters
  msg('Reading zone dasymetric population rasters...')
  rasters = list()
  extents = data.frame()
  for (c in 1:length(dasypops)) {
    file = dasypops[c]
    message('Reading ', file)
    r = raster(file)
    rasters = append(rasters, r)
    e = extent(r)
    extents = rbind(extents, data.frame(
      xmin = e[1], xmax = e[2],
      ymin = e[3], ymax = e[4]
    ))
  }
  # mapview(rasters[[3]])
  
  # Combine into a single regional population raster
  e = extent(c(
    min(extents$xmin), max(extents$xmax),
    min(extents$ymin), max(extents$ymax)
  ))
  res = res(rasters[[1]])
  crs = crs(rasters[[1]])
  rasters_p = rasters
  template = raster(extent(e), res = res, crs = crs)
  for (c in 1:length(dasypops)) {
    names(rasters_p[[c]]) = paste0('', gsub('dasypop_', '', names(rasters_p[[c]])))
    message('Reprojecting ', names(rasters_p[[c]]))
    rasters_p[[c]] = projectRaster(rasters_p[[c]], template, method = 'ngb')
  }
  
  # Only take counties (which include native land population) for combined population exposure
  county_rasters_p = rasters_p[grep('_county', sapply(rasters_p, function(x) names(x)), ignore.case = T)]
  for (c in 1:length(county_rasters_p)) {
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
  msg('Creating Exposed.Population layer...')
  contours_Ldn    = st_transform(contours_Ldn, crs = crs)
  contours_Lnight = st_transform(contours_Lnight, crs = crs)
  contours_Leq24  = st_transform(contours_Leq24, crs = crs)
  dasy_pop_cropped = crop(dasy_pop, as(contours_Ldn, 'Spatial'))
  dasy_pop_cropped = mask(dasy_pop_cropped, as(contours_Ldn, 'Spatial'))
  dasy_pop_cropped[dasy_pop_cropped == 0] = NA # set all 0 population cells to NA
  template = raster(extent(dasy_pop_cropped), res = res, crs = crs)
  pop_exposed = projectRaster(dasy_pop_cropped, template, method = 'ngb')
  names(pop_exposed) = 'Exposed.Population'
  # mapview(pop_exposed)
  
  # Rasterize the contour layers
  msg('Rasterizing contour layers...')
  contours_Ldn_raster    = rasterize(x = contours_Ldn, y = template, field = 'Level')
  names(contours_Ldn_raster) = 'Ldn'
  contours_Lnight_raster = rasterize(contours_Lnight, template, 'Level')
  names(contours_Lnight_raster) = 'Lnight'
  contours_Leq24_raster  = rasterize(contours_Leq24, template, 'Level')
  names(contours_Leq24_raster) = 'Leq24'
  
  pop_exposure_stack = stack(list(
      pop_exposed,
      contours_Ldn_raster,
      contours_Lnight_raster,
      contours_Leq24_raster
  ))
  # mapview(pop_exposure_stack)
  
  # Write layers to file
  filename = glue('{output_path}/pop_exposure_stack.grd')
  writeRaster(brick(pop_exposure_stack), filename = filename, overwrite = T)
  message('Created ', filename)
  
  filename = glue('{output_path}/pop_zones_stack.grd')
  writeRaster(brick(stack(rasters_p)), filename = filename, overwrite = T)
  message('Created ', filename)
}
