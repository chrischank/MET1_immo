###########################################################################
#Asset price exploration and prediction on 3 communes of Santiago de Chile#
#Maintainer: Christopher Chan                                             #
#Date: 2021-06-06                                                         #
#Version: 0.0.1                                                           #
###########################################################################

setwd("C:/Users/Chris/Dropbox/EAGLE_Assessments/MET1_Model/MET1_immo/")

pkgs <- c("tidyverse", "rgdal", "RStoolbox", "sf", "rasterVis", "ggmap", "viridis", "osmdata")

for (i in pkgs){
  if (!require(i, character.only = TRUE)){
    install.packages(i, dependencies = TRUE)
    library(i, dependencies=TRUE)
  }
}

vector_path <- file.path(getwd(), "Vector")
raster_path <- file.path(getwd(), "Raster")

# VECTOR PRE-PROCESSING ----

LIMITES <- file.path(vector_path, "BOUNDARIES_DPA_V0501_SIRGASCHILE_GCS", "LIMITES_DPA_V0501_SIRGASCHILE_GCS.shp") %>% 
  readOGR()
head(LIMITES)
str(LIMITES)

Master_street <- file.path(vector_path, "Master_Streets_2018-shp", "a20dbafa-3947-48be-bc38-574ea1e141332020329-1-mdiyb4.umz09.shp") %>% 
  readOGR
head(Master_street)
str(Master_street)

# AOI boundaries from OSM
AOI_las_condes <- osmdata::getbb("Las Condes", featuretype = "boundaries", format_out = "sf_polygon")
AOI_san_miguel <- osmdata::getbb("San Miguel", featuretype = "boundaries", format_out = "sf_polygon")
AOI_puente_alto <- osmdata::getbb("Puente Alto", featuretype = "boundaries", format_out = "sf_polygon")
plot(AOI_san_miguel)
