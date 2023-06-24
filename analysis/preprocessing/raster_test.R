source('analysis/spatial_distribution.R')

library(raster)
library(rgdal)

# Read the existing raster and shapefile layer
contours_Ldn = get_contours_Ldn()
contours_Ldn = contours_Ldn[as.numeric(contours_Ldn$Level)>=45,] # WHO health impact threshold
dasy_pop = raster('output_tifs/neon-dasy-WA-Island.tif')

# Set the projection of the shapefile to match the existing raster
contours_Ldn = st_transform(contours_Ldn, crs = projection(dasy_pop))

# Create an empty raster with the extent of the shapefile and the resolution of the existing raster, and set the projection of the empty raster to match the existing raster
empty_raster = raster(extent(contours_Ldn), res = c(res(dasy_pop)[1], res(dasy_pop)[2]))
projection(empty_raster) = projection(dasy_pop)

# Rasterize the shapefile layer using the empty raster
contours_Ldn_raster = rasterize(contours_Ldn, empty_raster, 'Level')

mapviewOptions(mapview.maxpixels = 50000000)
mapview(contours_Ldn_raster) + mapview(contours_Ldn) + mapview(dasy_pop)

dasy_pop_points = rasterToPoints(dasy_pop)
sum(as.data.frame(dasy_pop_points)[3]) # this should be the total population
Ldn_points = rasterToPoints(contours_Ldn_raster)
