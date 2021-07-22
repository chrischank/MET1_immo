###########################################################################
#Asset price exploration and prediction on 3 communes of Santiago de Chile#
#Maintainer: Christopher Chan                                             #
#Date: 2021-06-30                                                         #
#Version: 0.0.3                                                           #
###########################################################################

setwd("C:/Users/Chris/Dropbox/EAGLE_Assessments/MET1_Model/MET1_immo/")

pkgs <- c("tidyverse", "rgdal", "RStoolbox", "sf", "rasterVis", "ggmap", "viridis", "osmdata",
          "tmap", "RColorBrewer", "Hmics", "reticulate")

for (i in pkgs){
  if (!require(i, character.only = TRUE)){
    install.packages(i, dependencies = TRUE)
    library(i, dependencies=TRUE)
  }
}

figure_path <- file.path(getwd(), "Figure")
vector_path <- file.path(getwd(), "Vector")
raster_path <- file.path(getwd(), "Raster")

# VECTOR PRE-PROCESSING ----

# AOI boundaries from OSM

########################
#BBox Santiago de Chile#
########################

AOI_Santiago <- readOGR(file.path(vector_path, "AOI_Santiago.geojson"))
AOI_Santiago <- spTransform(AOI_Santiago, CRS("+init=epsg:9184"))

plot(AOI_Santiago)
  
################################
#BBox Las Condes & OSM features#
################################

AOI_las_condes <- osmdata::getbb("Las Condes", featuretype = "boundaries",
                                 display_name_contains = "Santiago",
                                 format_out = "sf_polygon") %>%
  st_transform(AOI_las_condes, crs = 9184)

# RESIDENTIAL (Accent)

AOI_LC_res <- opq(bbox = AOI_las_condes) %>% 
  add_osm_feature(key = "building", value = c("apartments", "detached",
                                              "semidetached_house", "house",
                                              "hut")) %>% 
  osmdata_sf()

glimpse(AOI_LC_res$osm_polygons)
glimpse(AOI_LC_res$osm_multipolygons)

AOI_LC_res <- bind_rows(st_cast(AOI_LC_res$osm_polygons, "MULTIPOLYGON"),
                        AOI_LC_res$osm_multipolygons) %>% 
  dplyr::select(name, osm_id, building)

AOI_LC_res <- st_transform(AOI_LC_res, crs = 9184)

#(tm_basemap(leaflet::providers$CartoDB.DarkMatter) +
#    tm_shape(AOI_LC_res) +
#    tm_sf(col = "building",
#          title = "LC_residential_type",
#          palette = RColorBrewer::brewer.pal("building", "Accent"),
#          tm_scale_bar())
#)

# AMENITIES (Paired)

AOI_LC_amy <- opq(bbox = AOI_las_condes) %>% 
  add_osm_feature(key = "amenity", value = c("college", "library",
                                              "university", "cafe",
                                              "restaurant", "bar", 
                                              "bank", "hospital", 
                                              "clinic", "arts_centre",
                                              "theatre", "fire_station", 
                                              "police", "place_of_worship",
                                              "marketplace", "conference_centre",
                                              "grave_yard")) %>% 
  osmdata_sf()

glimpse(AOI_LC_amy$osm_polygons)
glimpse(AOI_LC_amy$osm_multipolygons)

AOI_LC_amy <- bind_rows(st_cast(AOI_LC_amy$osm_polygons, "MULTIPOLYGON"),
                        AOI_LC_amy$osm_multipolygons) %>% 
  dplyr::select(name, osm_id, amenity)

AOI_LC_amy <- st_transform(AOI_LC_amy, crs = 9184)

#(tm_basemap(leaflet::providers$CartoDB.DarkMatter) +
#    tm_shape(AOI_LC_amy) +
#    tm_sf(col = "amenity",
#          title = "LC_amenity_type",
#          palette = RColorBrewer::brewer.pal("amenity", "Paired"),
#          tm_scale_bar())
#)


# LEISURE (Pastel1)

AOI_LC_les <- opq(bbox = AOI_las_condes) %>% 
  add_osm_feature(key = "leisure", value = c("golf_course", "nature_reserve",
                                             "park", "playground",
                                             "resort", "stadium",
                                             "swimming_pool", "swimming_area",
                                             "horse_riding", "garden")) %>% 
  osmdata_sf()

glimpse(AOI_LC_les$osm_polygons)
glimpse(AOI_LC_les$osm_multipolygons)

AOI_LC_les <- bind_rows(st_cast(AOI_LC_les$osm_polygons, "MULTIPOLYGON"),
                        AOI_LC_les$osm_multipolygons) %>% 
  dplyr::select(name, osm_id, leisure)

AOI_LC_les <- st_transform(AOI_LC_les, crs = 9184)

#(tm_basemap(leaflet::providers$CartoDB.DarkMatter) +
#    tm_shape(AOI_LC_les) +
#    tm_sf(col = "leisure",
#          title = "LC_leisure_type",
#          palette = RColorBrewer::brewer.pal("leisure", "Pastel1"),
#          tm_scale_bar())
#)

# STREETS

AOI_LC_Street <- opq(bbox = AOI_las_condes) %>% 
  add_osm_feature(key = "highway", value = c("motorway", "trunk",
                                             "primary")) %>% 
  osmdata_sf()

glimpse(AOI_LC_Street$osm_lines)

AOI_LC_Street <- AOI_LC_Street$osm_lines %>% 
  as_Spatial()


AOI_LC_Street <- spTransform(AOI_LC_Street, crs("+init=epsg:9184"))

################################
#BBox San Miguel & OSM features#
################################

AOI_san_miguel <- osmdata::getbb("San Miguel", featuretype = "boundaries",
                                 display_name_contains = "Santiago",
                                 format_out = "sf_polygon") %>%
  st_transform(AOI_san_miguel, crs = 9184)

# RESIDENTIAL

AOI_SM_res <- opq(bbox = AOI_san_miguel) %>% 
  add_osm_feature(key = "building", value = c("apartments", "detached",
                                              "semidetached_house", "house",
                                              "hut")) %>% 
  osmdata_sf()

glimpse(AOI_SM_res$osm_polygons)
glimpse(AOI_SM_res$osm_multipolygons)

AOI_SM_res <- bind_rows(st_cast(AOI_SM_res$osm_polygons, "MULTIPOLYGON"),
                        AOI_SM_res$osm_multipolygons) %>% 
  dplyr::select(name, osm_id, building)

AOI_SM_res <- st_transform(AOI_SM_res, crs = 9184)

plot(AOI_SM_res["building"])

#(tm_basemap(leaflet::providers$CartoDB.DarkMatter) + # add Stamen Terrain basemap
#    tm_shape(AOI_SM_res) + # add the sf
#    tm_sf(col = "building", # colour by green space type
#          title = "SM_residential_type", # no legend title
#          palette = RColorBrewer::brewer.pal("building", "Accent"), # custom fill colours
#          tm_scale_bar()) # add scale bar
#)

# AMENITIES

AOI_SM_amy <- opq(bbox = AOI_san_miguel) %>% 
  add_osm_feature(key = "amenity", value = c("college", "library",
                                             "university", "cafe",
                                             "restaurant", "bar", 
                                             "bank", "hospital", 
                                             "clinic", "arts_centre",
                                             "theatre", "fire_station", 
                                             "police", "place_of_worship",
                                             "marketplace", "conference_centre",
                                             "grave_yard")) %>% 
  osmdata_sf()

glimpse(AOI_SM_amy$osm_polygons)
glimpse(AOI_SM_amy$osm_multipolygons)

AOI_SM_amy <- bind_rows(st_cast(AOI_SM_amy$osm_polygons, "MULTIPOLYGON"),
                        AOI_SM_amy$osm_multipolygons) %>% 
  dplyr::select(name, osm_id, amenity)

AOI_SM_amy <- st_transform(AOI_SM_amy, crs = 9184)

#(tm_basemap(leaflet::providers$CartoDB.DarkMatte) +
#    tm_shape(AOI_SM_amy) +
#    tm_sf(col = "amenity",
#          title = "SM_amenity_type",
#          palette = RColorBrewer::brewer.pal("amenity", "Paired"),
#          tm_scale_bar())
#)

# LEISURE

AOI_SM_les <- opq(bbox = AOI_san_miguel) %>% 
  add_osm_feature(key = "leisure", value = c("golf_course", "nature_reserve",
                                             "park", "playground",
                                             "resort", "stadium",
                                             "swimming_pool", "swimming_area",
                                             "horse_riding", "garden")) %>% 
  osmdata_sf()

glimpse(AOI_SM_les$osm_polygons)
glimpse(AOI_SM_les$osm_multipolygons)

AOI_SM_les <- bind_rows(st_cast(AOI_SM_les$osm_polygons, "MULTIPOLYGON"),
                        AOI_SM_les$osm_multipolygons) %>% 
  dplyr::select(name, osm_id, leisure)

AOI_SM_les <- st_transform(AOI_SM_les, crs = 9184)

#(tm_basemap(leaflet::providers$CartoDB.DarkMatte) +
#    tm_shape(AOI_SM_les) +
#    tm_sf(col = "leisure",
#          title = "SM_leisure_type",
#          palette = RColorBrewer::brewer.pal("leisure", "Pastel1"),
#          tm_scale_bar())
#)

# STREETS

AOI_SM_Street <- opq(bbox = AOI_san_miguel) %>% 
  add_osm_feature(key = "highway", value = c("motorway", "trunk",
                                             "primary")) %>% 
  osmdata_sf()

glimpse(AOI_SM_Street$osm_lines)

AOI_SM_Street <- AOI_SM_Street$osm_lines %>% 
  as_Spatial()


AOI_SM_Street <- spTransform(AOI_SM_Street, crs("+init=epsg:9184"))


################################
#BBox Puente Alto & OSM features#
################################

AOI_puente_alto <- osmdata::getbb("Puente Alto", featuretype = "boundaries",
                                  display_name_contains = "Santiago",
                                  format_out = "sf_polygon") %>%
  st_transform(AOI_puente_alto, crs = 9184)

# RESIDENTIAL

AOI_PA_res <- opq(bbox = AOI_puente_alto) %>% 
  add_osm_feature(key = "building", value = c("apartments", "detached",
                                              "semidetached_house", "house",
                                              "hut")) %>% 
  osmdata_sf()

glimpse(AOI_PA_res$osm_polygons)
glimpse(AOI_PA_res$osm_multipolygons)

AOI_PA_res <- bind_rows(st_cast(AOI_PA_res$osm_polygons, "MULTIPOLYGON"),
                        AOI_PA_res$osm_multipolygons) %>% 
  dplyr::select(name, osm_id, building)

AOI_PA_res <- st_transform(AOI_PA_res, crs = 9184)

plot(AOI_PA_res["building"])

#(tm_basemap(leaflet::providers$CartoDB.DarkMatte) + # add Stamen Terrain basemap
#    tm_shape(AOI_PA_res) + # add the sf
#    tm_sf(col = "building", # colour by green space type
#          title = "PA_residential_type", # no legend title
#          palette = RColorBrewer::brewer.pal("building", "Accent"), # custom fill colours
#          tm_scale_bar()) # add scale bar
#)

# AMENITIES

AOI_PA_amy <- opq(bbox = AOI_puente_alto) %>% 
  add_osm_feature(key = "amenity", value = c("college", "library",
                                             "university", "cafe",
                                             "restaurant", "bar", 
                                             "bank", "hospital", 
                                             "clinic", "arts_centre",
                                             "theatre", "fire_station", 
                                             "police", "place_of_worship",
                                             "marketplace", "conference_centre",
                                             "grave_yard")) %>% 
  osmdata_sf()

glimpse(AOI_PA_amy$osm_polygons)
glimpse(AOI_pA_amy$osm_multipolygons)

AOI_PA_amy <- bind_rows(st_cast(AOI_PA_amy$osm_polygons, "MULTIPOLYGON"),
                        AOI_PA_amy$osm_multipolygons) %>% 
  dplyr::select(name, osm_id, amenity)

AOI_PA_amy <- st_transform(AOI_PA_amy, crs = 9184)

#(tm_basemap(leaflet::providers$CartoDB.DarkMatte) +
#    tm_shape(AOI_PA_amy) +
#    tm_sf(col = "amenity",
#          title = "PA_amenity_type",
#          palette = RColorBrewer::brewer.pal("amenity", "Paired"),
#          tm_scale_bar())
#)

# LEISURE

AOI_PA_les <- opq(bbox = AOI_puente_alto) %>% 
  add_osm_feature(key = "leisure", value = c("golf_course", "nature_reserve",
                                             "park", "playground",
                                             "resort", "stadium",
                                             "swimming_pool", "swimming_area",
                                             "horse_riding", "garden")) %>% 
  osmdata_sf()

glimpse(AOI_PA_les$osm_polygons)
glimpse(AOI_PA_les$osm_multipolygons)

AOI_PA_les <- bind_rows(st_cast(AOI_PA_les$osm_polygons, "MULTIPOLYGON"),
                        AOI_PA_les$osm_multipolygons) %>% 
  dplyr::select(name, osm_id, leisure)

AOI_PA_les <- st_transform(AOI_PA_les, crs = 9184)

#(tm_basemap(leaflet::providers$CartoDB.DarkMatte) +
#    tm_shape(AOI_PA_les) +
#    tm_sf(col = "leisure",
#          title = "PA_leisure_type",
#          palette = RColorBrewer::brewer.pal("amenity", "Paired"),
#          tm_scale_bar())
#)

# STREETS

AOI_PA_Street <- opq(bbox = AOI_puente_alto) %>% 
  add_osm_feature(key = "highway", value = c("motorway", "trunk",
                                             "primary")) %>% 
  osmdata_sf()

glimpse(AOI_PA_Street$osm_lines)

AOI_PA_Street <- AOI_PA_Street$osm_lines %>% 
  as_Spatial()


AOI_PA_Street <- spTransform(AOI_PA_Street, crs("+init=epsg:9184"))

# Write and read AOI locally as SPDFs

st_write(AOI_Santiago, dsn = file.path(vector_path, "AOI_Santiago.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_las_condes, dsn = file.path(vector_path, "AOI_LC.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_san_miguel, dsn = file.path(vector_path, "AOI_SM.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_puente_alto, dsn = file.path(vector_path, "AOI_PA.geojson"),
         driver = "GeoJSON", append = FALSE)

AOI_LC_Street$highway

writeOGR(AOI_LC_Street, layer = "highway", dsn = file.path(vector_path, "AOI_LC_Street.geojson"),
         driver = "GeoJSON", overwrite_layer = TRUE)
writeOGR(AOI_SM_Street, layer = "highway", dsn = file.path(vector_path, "AOI_SM_Street.geojson"),
         driver = "GeoJSON", overwrite_layer = TRUE)
writeOGR(AOI_PA_Street, layer = "highway", dsn = file.path(vector_path, "AOI_PA_Street.geojson"),
         driver = "GeoJSON", overwrite_layer = TRUE)

AOI_las_condes <- readOGR(file.path(vector_path, "AOI_LC.geojson"))
AOI_san_miguel <- readOGR(file.path(vector_path, "AOI_SM.geojson"))
AOI_puente_alto <- readOGR(file.path(vector_path, "AOI_PA.geojson"))
AOI_LC_Street <- readOGR(file.path(vector_path, "AOI_LC_Street.geojson"))
AOI_SM_Street <- readOGR(file.path(vector_path, "AOI_SM_Street.geojson"))
AOI_PA_Street <- readOGR(file.path(vector_path, "AOI_PA_Street.geojson"))

# Pre-processing for Census 2017, Crime, Base price

# Census-2017

census_path <- file.path(vector_path, "Censo2017_16R_ManzanaEntidad_CSV/Censo2017_Manzanas_write.csv")
census_2017 <- read_csv(census_path)

str(census_2017)
summary(census_2017)

censusF_2017 <- census_2017 %>% 
  groupby("COMUNA", "")

# Crime

crime <- readOGR(file.path(vector_path, "delitos_por_cuadrantePolygon.shp"))
summary(crime)

(crime_plot <- ggplot(crime, aes(x=)))


# Base price
base_price <- readOGR()

# RASTER PRE-PROCESSING ----

L2A_20210717_path <- file.path(raster_path, "MASK_S2A_MSIL2A_20210717T143731_N0301_R096_T19HCD_20210717T182511")
L2A_20210717_path %>% list.files()

#############################################
#run class_funs.py script to convert jp2tiff#
#############################################

# Unfortunately the AOI lies between 2 scenes
# Therefore, it is requried to load and mosaic 3 sets of 2 scenes

tif_20210717A_path <- file.path(raster_path, "20210717T143731_20210717T144707_T19HCC.tif")
tif_20210717A <- raster(tif_20210717A_path)

tif_20210717B_path <- file.path(raster_path, "20210717T143731_20210717T144707_T19HCD.tif")
tif_20210717B <- raster(tif_20210717B_path)

# Distribution of pixel

par(mfrow = c(1, 2))
boxplot(values(tif_20210717A))
boxplot(values(tif_20210717B))

##############################################
#Snow mask and restretch pixels before mosaic#
##############################################

# Normalised Difference Snow index for snow mask (Hall & Riggs, 2010)
# b11 & b8 for Sentinel 2
# b11 ~ 20m res.
# b8 ~ 10m res.

A_b11_SWIR <- raster(tif_20210717A_path, band = 11)
A_b8_NIR <- raster(tif_20210717A_path, band = 8)
B_b11_SWIR <- raster(tif_20210717B_path, band = 11)
B_b8_NIR <- raster(tif_20210717B_path, band = 8)

# Upsampling of b11 to 10m resolution before further computation

A_b11_SWIR <- resample(A_b11_SWIR, A_b8_NIR)
B_b11_SWIR <- resample(B_b11_SWIR, B_b8_NIR)

plot(A_b11_SWIR)
plot(A_b8_NIR)

# Create NDSI function

NDSI_fun <- function(NIR, SWIR){
  (NIR-SWIR)/(NIR+SWIR)
}

NDSI_20210717A <- NDSI_fun(A_b8_NIR, A_b11_SWIR)
NDSI_20210717B <- NDSI_fun(B_b8_NIR, B_b11_SWIR)

# Reclassify to remove -inf to 0.4
NDSI_20210717A <- reclassify(NDSI_20210717A, cbind(-Inf, 0.4, NA))
NDSI_20210717B <- reclassify(NDSI_20210717B, cbind(-Inf, 0.4, NA))

plot(NDSI_20210717A, col = RColorBrewer::brewer.pal(3, "Greys"))
plot(NDSI_20210717B, col = RColorBrewer::brewer.pal(3, "Greys"))

# Apply snow mask


# Mosaic

tif_20210717

RStoolbox::histMatch()


#ggRGB(RGB_20210717, r=3, g=2, b=1, stretch="hist")



