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

#########################

AOI_Santiago <- osmdata::getbb("Provincia de Santiago", featuretype = "boundaries",
                               format_out = "sf_polygon") %>%
  st_transform(AOI_Santiago, crs = 9184)

st_write(AOI_Santiago, dsn = file.path(vector_path, "AOI_Santiago.geojson"),
         driver = "GeoJSON", append = FALSE)

AOI_Santiago <- readOGR(file.path(vector_path, "AOI_Santiago.geojson"))

#########################

AOI_las_condes <- osmdata::getbb("Las Condes", featuretype = "boundaries",
                                 display_name_contains = "Santiago",
                                 format_out = "sf_polygon") %>%
  st_transform(AOI_las_condes, crs = 9184)

AOI_LC_res <- opq(bbox = AOI_las_condes) %>% 
  add_osm_feature(key = "building", value = c("apartments", "detached",
                                              "semidetached_house", "house",
                                              "hut")) %>% 
  osmdata_sf()

AOI_LC_res$osm_multipolygons$osm_id %>% 
  as_data_frame()

(ggplot() +
    geom_polygon(data = AOI_LC_res$osm_multipolygons$osm_id,
                 aes(x = x, y = y, group = group,
                     fill = AOI_LC_res$osm_multipolygons$building),
                 color = "black", size = 0.5) +
    theme_classic() +
    theme(legend.position="bottom") +
    theme(legend.title=element_blank()) + 
    xlab("Longitude") +
    ylab("Latitude") + 
    coord_quickmap())

##########################

AOI_san_miguel <- osmdata::getbb("San Miguel", featuretype = "boundaries",
                                 display_name_contains = "Santiago",
                                 format_out = "sf_polygon") %>%
  st_transform(AOI_san_miguel, crs = 9184)

###########################

AOI_puente_alto <- osmdata::getbb("Puente Alto", featuretype = "boundaries",
                                  display_name_contains = "Santiago",
                                  format_out = "sf_polygon") %>%
  st_transform(AOI_puente_alto, crs = 9184)

# Write and read AOI locally as SPDFs
st_write(AOI_las_condes, dsn = file.path(vector_path, "AOI_LC.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_san_miguel, dsn = file.path(vector_path, "AOI_SM.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_puente_alto, dsn = file.path(vector_path, "AOI_PA.geojson"),
         driver = "GeoJSON", append = FALSE)

AOI_las_condes <- readOGR(file.path(vector_path, "AOI_LC.geojson"))
AOI_san_miguel <- readOGR(file.path(vector_path, "AOI_SM.geojson"))
AOI_puente_alto <- readOGR(file.path(vector_path, "AOI_PA.geojson"))

plot(AOI_las_condes)
