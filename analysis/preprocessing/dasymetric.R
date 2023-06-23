# https://zenodo.org/record/7853206
# Load packages
library(tidycensus)
library(raster)
library(sf)
library(glue)
library(gdalUtils)
library(rslurm)
library(fasterize)

# Initial setup: file tree structure
path = paste0(getwd(), '/analysis/preprocessing/_output/')
direc = paste0(path, 'input_data/NLCD')
dir.create(direc)
dir.create(paste0(path, 'output_tifs'))
dir.create(paste0(path, 'temp_files'))

# Pre-processing: download NLCD rasters and create virtual raster
# https://www.mrlc.gov/data?f%5B0%5D=category%3AUrban%20Imperviousness
# install.packages("devtools")
# devtools:::install_github("gearslaboratory/gdalUtils")
library(gdalUtils)
imp_raster_imgfile = '/Users/giojacuzzi/Desktop/PHI Project Data/nlcd_2019_impervious_l48_20210604/nlcd_2019_impervious_l48_20210604.img'
imp_desc_raster_imgfile = '/Users/giojacuzzi/Desktop/PHI Project Data/nlcd_2019_impervious_descriptor_l48_20210604/nlcd_2019_impervious_descriptor_l48_20210604.img'

# Run gdalbuildvrt with all defaults to just create a VRT that points to the IMG without any modification
imp_raster_file <- file.path(direc, 'NLCD_2021_impervious.vrt')
imp_desc_raster_file <- file.path(direc, 'NLCD_2021_impervious_descriptor.vrt')
gdalbuildvrt(gdalfile = imp_raster_imgfile, output.vrt = imp_raster_file)
gdalbuildvrt(gdalfile = imp_desc_raster_imgfile, output.vrt = imp_desc_raster_file)

# Albers equal-area projection
aea <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

# Read Census Bureau API key and get 2021 ACS 5-year block group population estimates
# census_api_key(readLines('censusapikey.txt')) 
census_api_key('a9d9f05e0560c7fadaed6b4168bedc56d0e4686d')

stid = 'WA'
ctyid = 'Island'

pop <- get_acs(geography = 'block group', variables = 'B01003_001', 
               year = 2021, state= stid, county = ctyid, 
               geometry = TRUE)  

# Data QC: remove empty geometries from pop
pop <- pop[!is.na(st_dimension(pop)), ]

# Project population to Albers equal-area
pop.projected <- st_transform(pop, crs = aea)

library(mapview)
mapview(pop.projected)

# Use gdalwarp to extract the county area, from the NLCD impervious percentage raster, already in Albers projection
temp_polygon_filename <- as.character(glue("temp_files/county-{stid}-{ctyid}.gpkg"))
temp_nlcdraster_filename <- as.character(glue("temp_files/countynlcd-{stid}-{ctyid}.tif"))
st_write(st_union(pop.projected), dsn = temp_polygon_filename, driver = 'GPKG', append=FALSE)
gdalwarp(srcfile = imp_raster_file, dstfile = temp_nlcdraster_filename, cutline = temp_polygon_filename, crop_to_cutline = TRUE, tr = c(30, 30), dstnodata = "None")

lu <- raster(temp_nlcdraster_filename)
mapview(lu, maxpixels = 1963770)

# Get 2020 decennial block-level population counts
# Filter for only the blocks with 0 population, and project to Albers equal-area
zero.pop <- get_decennial(geography = 'block', variables = 'P1_001N', 
                          year = 2020, state = stid, county = ctyid, 
                          geometry = TRUE)
mapview(zero.pop, zcol='value')
# %>% 
#   filter(value == 0) %>% 
#   st_transform(crs = aea)
zero.pop <- zero.pop[zero.pop$value==0,]
zero.pop = st_transform(zero.pop, crs = aea)
mapview(zero.pop)

# Mask NLCD impervious raster to county boundaries
lu <- mask(lu, as(pop.projected, "Spatial"))
# Set pixels with impervious percentage <= 1% to 0
lu[lu <= 1] <- 0
# Scale impervious percentages between 0 and 1
lu.ratio <- lu/100
mapview(lu.ratio, maxpixels = 1963770)

# Set all pixels in zero-population blocks to 0
lu.ratio.zp <- mask(lu.ratio, as(zero.pop, "Spatial"), inverse = TRUE, updatevalue = 0)
mapview(lu.ratio.zp, maxpixels = 1963770)

############### ORIGINAL - GO TO TEST
# 
# # Load impervious surface descriptor dataset, mask all pixels outside the county to NA
# imp.surf.desc <- raster(imp_desc_raster_file)
# imp.surf.crop <- crop(imp.surf.desc, as(pop.projected, "Spatial")) 
# mapview(imp.surf.crop)
# imp.surf.mask <- mask(imp.surf.crop, as(pop.projected, "Spatial"))
# mapview(imp.surf.mask)
# 
# # Mask out primary, secondary, and urban tertiary roads
# # Reclassify: set classes 1-6 (road) to 1 and classes 7-14 to NA (non=road)
# # reclass.table <- matrix(c(1,6,1,7,14,NA), ncol = 3, byrow = TRUE) 
# reclass.table <- matrix(c(1,6,1,7,26,NA), ncol = 3, byrow = TRUE)
# # Reclassify descriptor file and reproject it.
# imp.roads <- reclassify(imp.surf.mask, reclass.table, right = NA)
# imp.roads.p <- projectRaster(imp.roads, lu.ratio.zp, method = 'ngb') 

############# TEST
imp_desc_raster_imgfile = '/Users/giojacuzzi/Desktop/PHI Project Data/nlcd_2019_impervious_descriptor_l48_20210604/nlcd_2019_impervious_descriptor_l48_20210604.img'
imp.surf.desc = raster(imp_desc_raster_imgfile, band = 1, values=F)
imp.surf.crop = crop(imp.surf.desc, as(pop.projected, "Spatial"))

# 20 - Primary road
# 21 - Secondary road
# 22 - Tertiary road
# attr(imp.surf.crop, 'data')

data = slot(attr(imp.surf.crop, 'data'), 'attributes')[[1]]
data = data[data$Class_Names %in% c('Primary road', 'Secondary road', 'Tertiary road'), ]
slot(attr(imp.surf.crop, 'data'), 'attributes')[[1]] = data

slot(attr(imp.surf.crop, 'data'), 'attributes')[[1]]$Class_Names = replace(slot(attr(imp.surf.crop, 'data'), 'attributes')[[1]]$Class_Names, !(slot(attr(imp.surf.crop, 'data'), 'attributes')[[1]]$Class_Names %in% c('Primary road', 'Secondary road', 'Tertiary road')), NA)
mapview(imp.surf.crop, maxpixels = 1964775)

imp.surf.mask = mask(imp.surf.crop, as(pop.projected, "Spatial"))
mapview(imp.surf.mask, maxpixels = 1964775)

imp.roads = deratify(imp.surf.mask, "Class_Names")
imp.roads = reclassify(imp.roads, matrix(c(1,3,1), ncol = 3, byrow = TRUE), right = NA)
mapview(imp.roads, maxpixels = 1964775)
imp.roads.p = projectRaster(imp.roads, lu.ratio.zp, method = 'ngb') 
mapview(imp.roads.p, maxpixels = 5000000)

####### ORIGINAL

# Set all road pixels to 0 (all non-NA values in imp.roads.p)
RISA <- overlay(lu.ratio.zp, imp.roads.p, fun = function(x, y) {
  x[!is.na(y[])] <- 0
  return(x)
})

# Get the block group-level sum of the remaining impervious surface pixels
RISA.sum <- raster::extract(RISA, as(pop.projected,"Spatial"), fun=sum, na.rm=TRUE, df=TRUE)
# Rasterize the block group population estimates and impervious surface pixel sums
pop.df <- cbind(pop.projected, RISA.sum$layer)
bg.sum.pop <- fasterize(pop.projected, RISA, field = "estimate")
bg.sum.RISA <- fasterize(pop.df, RISA, field = "RISA.sum.layer")

# Generate density (people/30 m pixel) and write to file.
dasy.pop <- (bg.sum.pop/bg.sum.RISA) * RISA
stopifnot(cellStats(dasy.pop, 'sum') == sum(pop$estimate)) # total population
mapview(reclassify(dasy.pop, cbind(-Inf, 1e-06, NA), right=F), maxpixels = 50000000)
filename <- glue("output_tifs/neon-dasy-{stid}-{ctyid}.tif")
writeRaster(dasy.pop, filename, overwrite = TRUE, NAflag = -9999) 
