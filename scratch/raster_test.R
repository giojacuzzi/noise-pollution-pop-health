source('analysis/spatial_distribution.R')

library(raster)
library(rgdal)
library(mapview)
mapviewOptions(mapview.maxpixels = 50000000)
options(tigris_use_cache = T)

input_path = paste0(here::here(), '/analysis/preprocessing/_output')
output_path = paste0(here::here(), '/analysis/_output')
county_dasypops = list.files(input_path, pattern = paste0('.*dasypop.tif'), full.names = T)

# DEBUG
dasy_pop_file = county_dasypops[1]

# Read the dasymetric population raster and noise contour shapefile layer
contours_Ldn = get_contours_Ldn()
contours_Ldn = contours_Ldn[as.numeric(contours_Ldn$Level)>=45,] # WHO health impact threshold
dasy_pop = raster(dasy_pop_file)

# Set the projection of the contours to match the dasy population raster
contours_Ldn = st_transform(contours_Ldn, crs = projection(dasy_pop))

# Create an empty raster with the extent of the contours and the resolution of dasy_pop, and set the projection of the empty raster to match dasy_pop
empty_raster = raster(extent(contours_Ldn), res = c(res(dasy_pop)[1], res(dasy_pop)[2]))
projection(empty_raster) = projection(dasy_pop)

# Rasterize the contour layer using the empty raster
contours_Ldn_raster = rasterize(contours_Ldn, empty_raster, 'Level')
stopifnot(res(dasy_pop) == res(contours_Ldn_raster))

mapview(contours_Ldn_raster) + mapview(dasy_pop)

######################################################

# dasy_pop_points = rasterToPoints(dasy_pop)
# sum(as.data.frame(dasy_pop_points)[3]) # this should be the total population
# Ldn_points = rasterToPoints(contours_Ldn_raster)

# Annoyance test

# Reproject population and exposure to the same containing origin and extent
e = extent(c(
  min(extent(dasy_pop)[1], extent(contours_Ldn_raster)[1]), # xmin
  max(extent(dasy_pop)[2], extent(contours_Ldn_raster)[2]), # xmax
  min(extent(dasy_pop)[3], extent(contours_Ldn_raster)[3]), # ymin
  max(extent(dasy_pop)[4], extent(contours_Ldn_raster)[4]) # ymax
))
r = raster(extent(e), res = res(dasy_pop), crs = crs(dasy_pop))

p_dasy_pop = projectRaster(dasy_pop, r, method = 'ngb')
p_contours_Ldn_raster = projectRaster(contours_Ldn_raster, r, method = 'ngb')

p_dasy_pop[p_dasy_pop == 0] = NA # set all 0 population cells to NA
p_contours_Ldn_raster[is.na(p_contours_Ldn_raster)] = 0 # set all NA exposure cells to 0

stopifnot(extent(p_dasy_pop) == extent(p_contours_Ldn_raster))
stopifnot(origin(p_dasy_pop) == origin(p_contours_Ldn_raster))

percent_HA = calc(p_contours_Ldn_raster, fun=exp_resp_WHO_bounded)
estimated_pop_HA = percent_HA * 0.01 * p_dasy_pop
estimated_pop_HA[estimated_pop_HA == 0] = NA

# Number of people estimated to be highly annoyed (16396)
npop_HA = cellStats(estimated_pop_HA, 'sum')
percent_island_county_HA = npop_HA / cellStats(dasy_pop, 'sum')
