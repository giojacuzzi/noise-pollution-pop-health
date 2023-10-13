## Save package names as a vector of strings
pkgs <- c("dplyr", "ggplot2", "ggnewscale", "ggsn", "osmdata", "sf", "z11") 

## Install uninstalled packages
if (!("z11" %in% installed.packages()))
  remotes::install_github("StefanJuenger/z11")
lapply(pkgs[!(pkgs %in% installed.packages())], install.packages)

## Load all packages to library and adjust options
lapply(pkgs, library, character.only = TRUE)

mannheim <-
  osmdata::getbb("Mannheim") %>% 
  osmdata::opq(timeout = 25*100) %>%
  osmdata::add_osm_feature(
    key = "admin_level", 
    value = "6"
  ) %>% 
  osmdata::osmdata_sf() %$% 
  osm_multipolygons %>% 
  dplyr::filter(name == "Mannheim") %>% # filter on city level
  dplyr::select(geometry) %>%
  sf::st_transform(3035) 

mannheim