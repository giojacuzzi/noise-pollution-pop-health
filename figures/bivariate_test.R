# library("biscale")
# 
# input_path = paste0(here::here(), '/analysis/preprocessing/_output')
# output_path = paste0(here::here(), '/figures/_output')
# pop_exposure_stack = stack(glue('{input_path}/pop_exposure_stack.grd'))
# 
# # transform the raster to xyz and a sf object
# urb_mad <- as.data.frame(pop_exposure_stack[['Exposed.Population']], xy = TRUE, na.rm = TRUE) %>%
#   st_as_sf(coords = c("x", "y"), crs = crs)
# 
# ldny <- as.data.frame(pop_exposure_stack[['Ldn']], xy = TRUE, na.rm = TRUE) %>%
#   st_as_sf(coords = c("x", "y"), crs = crs)
# 
# testy = st_join(urb_mad, ldny)
# 
# # add the columns of the coordinates
# urb_mad <- urb_mad %>% rename(urb = 1) %>% cbind(st_coordinates(urb_mad))
# 
# ldny <- ldny %>% rename(urb = 1) %>% cbind(st_coordinates(ldny))
# 
# testy = full_join(urb_mad, ldny, by = c('X', 'Y'))

source('global.R')
library(bivariatemaps)
library(classInt)
library(raster)
library(rgdal)
library(dismo)
library(XML)
library(maps)
library(sp)

input_path = paste0(here::here(), '/analysis/preprocessing/_output')
output_path = paste0(here::here(), '/figures/_output')

colmat = function(nquantiles=10, upperleft=rgb(0,150,235, maxColorValue=255), upperright=rgb(130,0,80, maxColorValue=255), bottomleft="grey", bottomright=rgb(255,230,15, maxColorValue=255), xlab="x label", ylab="y label") {
  my.data<-seq(0,1,.01)
  my.class<-classIntervals(my.data,n=nquantiles,style="quantile")
  my.pal.1<-findColours(my.class,c(upperleft,bottomleft))
  my.pal.2<-findColours(my.class,c(upperright, bottomright))
  col.matrix<-matrix(nrow = 101, ncol = 101, NA)
  for(i in 1:101){
    my.col<-c(paste(my.pal.1[i]),paste(my.pal.2[i]))
    col.matrix[102-i,]<-findColours(my.class,my.col)}
  plot(c(1,1),pch=19,col=my.pal.1, cex=0.5,xlim=c(0,1),ylim=c(0,1),frame.plot=F, xlab=xlab, ylab=ylab,cex.lab=1.3)
  for(i in 1:101){
    col.temp<-col.matrix[i-1,]
    points(my.data,rep((i-1)/100,101),pch=15,col=col.temp, cex=1)}
  seqs<-seq(0,100,(100/nquantiles))
  seqs[1]<-1
  col.matrix<-col.matrix[c(seqs), c(seqs)]
}

pop_exposure_stack = stack(glue('{input_path}/pop_exposure_stack.grd'))
exposed_pop = pop_exposure_stack[['Exposed.Population']]
ldn_db = pop_exposure_stack[['Ldn']]

bounds_x = c(-123.0, -122.32) # [min, max]
bounds_y = c(48.05, 48.55)
bounds = data.frame(
  x = c(bounds_x[1], bounds_x[1], bounds_x[2], bounds_x[2]),
  y = c(bounds_y[1], bounds_y[2], bounds_y[2], bounds_y[1])
)

exposed_pop = projectRaster(exposed_pop, crs = projection(wa_counties_cb))
ldn_db = projectRaster(ldn_db, crs = projection(wa_counties_cb))

e = as(extent(bounds_x[1], bounds_x[2], bounds_y[1], bounds_y[2]), 'SpatialPolygons')
crs(e) = projection(wa_counties_cb)

exposed_pop = crop(exposed_pop, e)
ldn_db = crop(ldn_db, e)

ldn_db[] <- ldn_db[] - 40 # offset
ldn_db[is.na(ldn_db[])] <- 0

exposed_pop[is.na(exposed_pop[])] <- 0

nquantiles = 11
# col.matrix<-colmat(nquantiles=3, upperleft="blue", upperright="yellow", bottomleft="green", bottomright="red", xlab="My x label", ylab="My y label")
col.matrix <- colmat(nquantiles = nquantiles, upperleft='blue', upperright='#4B0082', bottomleft='gray', bottomright='red')

## bivariate.map
rasterx = ldn_db
rastery = exposed_pop
quanmean<-getValues(rasterx)
temp<-data.frame(quanmean, quantile=rep(NA, length(quanmean)))
brks<-with(temp, quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
brks
##
brks = seq(0, 50, (50-0)/(nquantiles)) # seq(40, 90, 5) - 40
brks
##
r1<-within(temp, quantile <- cut(quanmean, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
quantr<-data.frame(r1[,2]) 
quanvar<-getValues(rastery)
temp<-data.frame(quanvar, quantile=rep(NA, length(quanvar)))
brks<-with(temp, quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
brks
##
brks = seq(0, 14, (14-0)/(nquantiles))
brks
##
r2<-within(temp, quantile <- cut(quanvar, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
quantr2<-data.frame(r2[,2])
as.numeric.factor<-function(x) {as.numeric(levels(x))[x]}
col.matrix2<-col.matrix
cn<-unique(col.matrix)
for(i in 1:length(col.matrix2)){
  ifelse(is.na(col.matrix2[i]),col.matrix2[i]<-1,col.matrix2[i]<-which(col.matrix2[i]==cn)[1])}
cols<-numeric(length(quantr[,1]))
for(i in 1:length(quantr[,1])){
  # message(i)
  a<-as.numeric.factor(quantr[i,1])
  b<-as.numeric.factor(quantr2[i,1])
  cols[i]<-as.numeric(col.matrix2[b,a])}
# r<-rasterx
rasterx[1:length(rasterx)]<-cols
# return(r)

plot(rasterx, col=as.vector(col.matrix))

######################## https://dominicroye.github.io/en/2021/bivariate-dasymetric-map/

library(tidyverse)
library(sf)
library(readxl)
library(biscale)
library(patchwork)
library(raster)
library(sysfonts)
library(showtext)
library(raster)

# raster of CORINE LAND COVER 2018
urb <- raster("/Users/giojacuzzi/Downloads/bivariate/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")

# income data and Gini index
renta <- read_excel("/Users/giojacuzzi/Downloads/bivariate/renta/30824.xlsx")
gini <- read_excel("/Users/giojacuzzi/Downloads/bivariate/renta/37677.xlsx")

# census boundaries
limits <- read_sf("/Users/giojacuzzi/Downloads/bivariate/SECC_CE_20200101/SECC_CE_20200101.shp") 

# filter the Autonomous Community of Madrid
limits <- filter(limits, NCA == "Comunidad de Madrid")

# obtain the municipal limits
mun_limit <- group_by(limits, CUMUN) %>% summarise()

# project the limits
limits_prj <- st_transform(limits, projection(urb))

# crop and mask 
urb_mad <- crop(urb, limits_prj) %>%  mask(limits_prj)

# remove non-urban pixels
urb_mad[!urb_mad %in% 1:2] <- NA 

# plot the raster
plot(urb_mad)

# project
urb_mad <- projectRaster(urb_mad, crs = CRS("+proj=longlat +datum=WGS84 +no_defs"))

# transform the raster to xyz and a sf object
urb_mad <- as.data.frame(urb_mad, xy = TRUE, na.rm = TRUE) %>% st_as_sf(coords = c("x", "y"), crs = 4326)

# add the columns of the coordinates
urb_mad <- urb_mad %>% rename(urb = 1) %>% cbind(st_coordinates(urb_mad))

## income and Gini index data
renta_sec <- mutate(renta, NATCODE = str_extract(CUSEC, "[0-9]{5,10}"), 
                    nc_len = str_length(NATCODE),
                    mun_name = str_remove(CUSEC, NATCODE) %>% str_trim()) %>%
  filter(nc_len > 5)

gini_sec <- mutate(gini, NATCODE = str_extract(CUSEC, "[0-9]{5,10}"), 
                   nc_len = str_length(NATCODE),
                   mun_name = str_remove(CUSEC, NATCODE) %>% str_trim()) %>%
  filter(nc_len > 5)

# join both the income and Gini tables with the census limits
mad <- left_join(limits, renta_sec, by = c("CUSEC"="NATCODE")) %>% 
  left_join(gini_sec, by = c("CUSEC"="NATCODE"))

# convert selected columns to numeric
mad <- mutate_at(mad, c(23:27, 30:31), as.numeric)

# create bivariate classification
mapbivar <- bi_class(mad, GINI_2017, RNMP_2017, style = "quantile", dim = 3) %>% 
  mutate(bi_class = ifelse(str_detect(bi_class, "NA"), NA, bi_class))

# results
head(dplyr::select(mapbivar, GINI_2017, RNMP_2017, bi_class))

# redistribute urban pixels to inequality
mapdasi <- st_join(urb_mad, st_transform(mapbivar, 4326))

# bivariate legend
legend2 <- bi_legend(pal = "DkViolet",
                     dim = 3,
                     xlab = "Higher inequality",
                     ylab = "Higher income",
                     size = 9)


# download font
font_add_google("Montserrat", "Montserrat")
showtext_auto()

p2 <- ggplot(mapdasi) + 
  geom_tile(aes(X, Y, 
                fill = bi_class), 
            show.legend = FALSE) +
  geom_sf(data = mun_limit,  
          color = "grey80", 
          fill = NA, 
          size = 0.2) +
  annotation_custom(ggplotGrob(legend2), 
                    xmin = -3.25, xmax = -2.65,
                    ymin = 40.55, ymax = 40.95) +
  bi_scale_fill(pal = "DkViolet", 
                dim = 3, 
                na.value = "grey90") +
  labs(title = "dasymetric", x = "", y ="") +
  bi_theme() +
  theme(plot.title = element_text(family = "Montserrat", size = 30, face = "bold")) +
  coord_sf(crs = 4326); p2

#################### https://chris-prener.github.io/biscale/articles/rasters.html

library(biscale)   # bivariate mapping
library(cowplot)   # combine ggplot2 objects
library(faux)      # create simulated data
library(ggplot2)   # create maps
library(raster)    # work with raster data

DSM_HARV <- raster("https://github.com/chris-prener/biscale/raw/main/data-raw/NEON_HARV_dsmCrop.tif")
neon_harv <- as.data.frame(DSM_HARV, xy = TRUE)

## create simulated data
neon_harv$sim <- rnorm_pre(neon_harv$y, mu = 10, sd = 2, r = 0.7, empirical = TRUE)

## rename NEON_HARV_dsmCrop
neon_harv$ele <- neon_harv$NEON_HARV_dsmCrop

## reorder variables
neon_harv <- subset(neon_harv, select = c(ele, sim, x, y))

neon_harv <- bi_class(neon_harv, x = ele, y = sim, style = "quantile")

map <- ggplot() +
  geom_raster(data = neon_harv , aes(x = x, y = y, fill = bi_class)) +
  bi_scale_fill(pal = "DkBlue") +
  coord_quickmap() +
  labs(
    title = "Harvard Forest",
    x = "",
    y = "",
    caption = "Data via NEON Spatio-temporal Teaching Data Subset"
  ) +
  bi_theme(base_size = 16) +
  theme(legend.position="none"); map

legend <- bi_legend(pal = "DkBlue",
                    xlab = "Higher Elevation ",
                    ylab = "Higher Simulated Var ",
                    size = 6)

## construct final plot
finalPlot <-  plot_grid(
  map, legend,
  rel_widths = c(1, .4),
  nrow = 1
)

## print final plot
finalPlot

############

source('global.R')
library(biscale)   # bivariate mapping
library(cowplot)   # combine ggplot2 objects
library(faux)      # create simulated data
library(ggplot2)   # create maps
library(raster)    # work with raster data
library(tigris)
options(tigris_use_cache = T)

input_path = paste0(here::here(), '/analysis/preprocessing/_output')
output_path = paste0(here::here(), '/figures/_output')

pop_exposure_stack = stack(glue('{input_path}/pop_exposure_stack.grd'))
exposed_pop = pop_exposure_stack[['Exposed.Population']]
ldn_db = pop_exposure_stack[['Ldn']]

wa_counties_cb = counties(state = 'WA', cb = T)

exposed_pop = projectRaster(exposed_pop, crs = projection(wa_counties_cb))
ldn_db = projectRaster(ldn_db, crs = projection(wa_counties_cb))

bounds_x = c(-123.0, -122.32) # [min, max]
bounds_y = c(48.05, 48.55)
bounds = data.frame(
  x = c(bounds_x[1], bounds_x[1], bounds_x[2], bounds_x[2]),
  y = c(bounds_y[1], bounds_y[2], bounds_y[2], bounds_y[1])
)

e = as(extent(bounds_x[1], bounds_x[2], bounds_y[1], bounds_y[2]), 'SpatialPolygons')
crs(e) = projection(wa_counties_cb)

exposed_pop = crop(exposed_pop, e)
ldn_db = crop(ldn_db, e)

ndim = 5 #11

DSM_HARV <- raster("https://github.com/chris-prener/biscale/raw/main/data-raw/NEON_HARV_dsmCrop.tif")
exposed_pop[is.na(exposed_pop[])] = 0
ldn_testy = ldn_db
ldn_testy[] <- ldn_testy[] - 40 # offset
ldn_testy[is.na(ldn_testy[])] = 0

neon_harv <- as.data.frame(DSM_HARV, xy = TRUE)
exp_df = as.data.frame(exposed_pop, xy = T)
ldn_df = as.data.frame(ldn_testy, xy = T)

## create simulated data, rename NEON_HARV_dsmCrop
neon_harv$sim <- rnorm_pre(neon_harv$y, mu = 10, sd = 2, r = 0.7, empirical = TRUE)
neon_harv$ele <- neon_harv$NEON_HARV_dsmCrop

# mergy = merge(x = exp_df, y = ldn_df, by = c('x', 'y'), all = T)
all(exp_df$x == ldn_df$x)
all(exp_df$y == ldn_df$y)

exp_df$Ldn = ldn_df$Ldn

## reorder variables
neon_harv <- subset(neon_harv, select = c(ele, sim, x, y))
exp_df <- subset(exp_df, select = c(Exposed.Population, Ldn, x, y))

# we’ll compare our simulated data with the elevation data in the raster:
neon_harv <- bi_class(neon_harv, x = ele, y = sim, style = "quantile")
exp_bi <- bi_class(exp_df, x = Exposed.Population, y = Ldn, style = "equal", dim = ndim)

map <- ggplot() +
  geom_raster(data = neon_harv , aes(x = x, y = y, fill = bi_class)) +
  bi_scale_fill(pal = "DkBlue") +
  coord_quickmap() +
  labs(
    title = "Harvard Forest",
    x = "",
    y = "",
    caption = "Data via NEON Spatio-temporal Teaching Data Subset"
  ) +
  bi_theme(base_size = 16) +
  theme(legend.position="none"); map

legend <- bi_legend(pal = "DkBlue",
                    xlab = "Higher Elevation / More Population",
                    ylab = "Higher Simulated Var / More Noise",
                    size = 6); legend

custom_pal <- c(
  "1-1" = "#d3d3d3", # low x, low y
  "2-1" = "#b6cdcd",
  "3-1" = "#97c5c5",
  "4-1" = "#75bebe",
  "5-1" = "#52b6b6", # high x, low y
  "1-2" = "#cab6c5",
  "2-2" = "#aeb0bf",
  "3-2" = "#91aab9",
  "4-2" = "#70a4b2",
  "5-2" = "#4e9daa",
  "1-3" = "#c098b9",
  "2-3" = "#a593b3",
  "3-3" = "#898ead",
  "4-3" = "#6b89a6",
  "5-3" = "#4a839f",
  "1-4" = "#b77aab",
  "2-4" = "#9e76a6",
  "3-4" = "#8372a0",
  "4-4" = "#666e9a",
  "5-4" = "#476993",
  "1-5" = "#ad5b9c", # low x, high y
  "2-5" = "#955898",
  "3-5" = "#7c5592",
  "4-5" = "#60528d",
  "5-5" = "#434e87" # high x, high y
)

map <- ggplot() +
  geom_raster(data = exp_bi , aes(x = x, y = y, fill = bi_class)) +
  bi_scale_fill(pal = custom_pal, dim = ndim) +
  coord_quickmap() +
  labs(
    title = "Harvard Forest",
    x = "",
    y = "",
    caption = "Data via NEON Spatio-temporal Teaching Data Subset"
  ) +
  bi_theme(base_size = 16) +
  theme(legend.position="none"); map

legend <- bi_legend(pal = custom_pal,
                    dim = ndim,
                    xlab = "Higher Elevation / More Population",
                    ylab = "Higher Simulated Var / More Noise",
                    size = 6); legend

## construct final plot
finalPlot <-  plot_grid(
  map, legend,
  rel_widths = c(1, .4),
  nrow = 1
)

## print final plot
finalPlot

