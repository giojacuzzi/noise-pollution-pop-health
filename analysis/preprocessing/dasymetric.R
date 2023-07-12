# Dasymetric population mapping framework:
# https://zenodo.org/record/7853206
# NLCD rasters downloaded from:
# https://www.mrlc.gov/data?f%5B0%5D=category%3AUrban%20Imperviousness

# Load packages
library(tidycensus)
library(raster)
library(sf)
library(glue)
library(gdalUtils)
library(rslurm)
library(fasterize)
library(mapview)
mapviewOptions(mapview.maxpixels = 50000000)
options(tigris_use_cache = T)
census_api_key('a9d9f05e0560c7fadaed6b4168bedc56d0e4686d')

# Initial setup: file tree structure
input_path = '/Users/giojacuzzi/Desktop/PHI Project Data/gis/NLCD'
output_path = paste0(here::here(), '/analysis/preprocessing/_output')
ctyids = c('Jefferson', 'San Juan', 'Snohomish', 'Island', 'Skagit')

# DEBUG
ctyid = ctyids[4]
message('Processing county ', ctyid)

# Create virtual raster VRTs pointing to IMGs without any modification
imp_raster_imgfile = glue('{input_path}/nlcd_2019_impervious_l48_20210604.img')
imp_raster_file = glue('{output_path}/{ctyid}_impervious.vrt')
gdalbuildvrt(
  gdalfile = imp_raster_imgfile,
  output.vrt = imp_raster_file
)

imp_desc_raster_imgfile = glue('{input_path}/nlcd_2019_impervious_descriptor_l48_20210604.img')
imp_desc_raster_file = glue('{output_path}/{ctyid}_impervious_descriptor.vrt')
gdalbuildvrt(
  gdalfile = imp_desc_raster_imgfile,
  output.vrt = imp_desc_raster_file
)

# Albers equal-area projection
aea = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

# Get 2021 ACS 5-year block group population estimates
pop = get_acs(geography = 'block group', variables = 'B01003_001', year = 2021, state = 'WA', county = ctyid, geometry = T)  

# Remove empty geometries and project to Albers equal-area
pop = pop[!is.na(st_dimension(pop)), ]
pop.projected = st_transform(pop, crs = aea)
mapview(pop.projected)

# Use gdalwarp to extract the county area from the NLCD impervious percentage raster (already in Albers projection)
polygon_file = glue('{output_path}/{ctyid}_area.gpkg')
raster_file = glue('{output_path}/{ctyid}_impervious.tif')
st_write(st_union(pop.projected), dsn = polygon_file, driver = 'GPKG', append = F)
gdalwarp(
  srcfile = imp_raster_file, dstfile = raster_file,
  cutline = polygon_file, crop_to_cutline = T,
  tr = c(30, 30), dstnodata = 'None'
)

lu = raster(raster_file)
mapview(lu)

# Get 2020 decennial block-level population counts
# Filter for only the blocks with 0 population, and project to Albers equal-area
zero.pop = get_decennial(geography = 'block', variables = 'P1_001N', year = 2020, state = 'WA', county = ctyid, geometry = T)
mapview(zero.pop, zcol='value')
zero.pop = zero.pop[zero.pop$value==0,]
zero.pop = st_transform(zero.pop, crs = aea)
mapview(zero.pop)

# Mask impervious raster to county boundaries
lu = mask(lu, as(pop.projected, 'Spatial'))
# Set pixels with impervious percentage <= 1% to 0
lu[lu <= 1] = 0
# Scale impervious percentages between 0 and 1
lu.ratio = lu/100
# Set all pixels in zero-population blocks to 0
lu.ratio.zp = mask(lu.ratio, as(zero.pop, 'Spatial'), inverse = T, updatevalue = 0)
mapview(lu.ratio.zp)

# Load impervious surface descriptor dataset, mask all pixels outside the county to NA
imp.surf.desc = raster(imp_desc_raster_imgfile, band = 1, values = F)
imp.surf.crop = crop(imp.surf.desc, as(pop.projected, 'Spatial'))

# Mask out primary (20), secondary (21), and urban tertiary (22) roads
# attr(imp.surf.crop, 'data')
data = slot(attr(imp.surf.crop, 'data'), 'attributes')[[1]]
data = data[data$Class_Names %in% c('Primary road', 'Secondary road', 'Tertiary road'), ]
slot(attr(imp.surf.crop, 'data'), 'attributes')[[1]] = data

slot(attr(imp.surf.crop, 'data'), 'attributes')[[1]]$Class_Names = replace(slot(attr(imp.surf.crop, 'data'), 'attributes')[[1]]$Class_Names, !(slot(attr(imp.surf.crop, 'data'), 'attributes')[[1]]$Class_Names %in% c('Primary road', 'Secondary road', 'Tertiary road')), NA)
imp.surf.mask = mask(imp.surf.crop, as(pop.projected, 'Spatial'))
mapview(imp.surf.mask)

# Reclassify road descriptors as '1' and reproject
imp.roads = deratify(imp.surf.mask, 'Class_Names')
imp.roads = reclassify(imp.roads, matrix(c(1,3,1), ncol = 3, byrow = T), right = NA)
imp.roads.p = projectRaster(imp.roads, lu.ratio.zp, method = 'ngb') 
mapview(imp.roads.p)

# Set all road pixels to 0 (all non-NA values in imp.roads.p)
RISA <- overlay(lu.ratio.zp, imp.roads.p, fun = function(x, y) {
  x[!is.na(y[])] = 0
  return(x)
})

# Get the block group-level sum of the remaining impervious surface pixels
RISA.sum = raster::extract(RISA, as(pop.projected, 'Spatial'), fun = sum, na.rm = T, df = T)
# Rasterize the block group population estimates and impervious surface pixel sums
pop.df = cbind(pop.projected, RISA.sum$layer)
bg.sum.pop  = fasterize(pop.projected, RISA, field = 'estimate')
bg.sum.RISA = fasterize(pop.df, RISA, field = 'RISA.sum.layer')

# Generate density (people/30 m pixel) and write to file.
dasy.pop = (bg.sum.pop/bg.sum.RISA) * RISA
stopifnot(cellStats(dasy.pop, 'sum') == sum(pop$estimate)) # total population
mapview(reclassify(dasy.pop, cbind(-Inf, 1e-06, NA), right=F))
filename = glue('{output_path}/{ctyid}_dasypop.tif')
writeRaster(dasy.pop, filename, overwrite = T, NAflag = -9999) 
message('Created ', filename)
