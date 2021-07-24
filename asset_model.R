###########################################################################
#Asset price exploration and prediction on 3 communes of Santiago de Chile#
#Maintainer: Christopher Chan                                             #
#Date: 2021-07-24                                                         #
#Version: 0.0.4                                                           #
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

# Census_2017 Data

census_2017 <- file.path(vector_path, "Censo2017_16R_ManzanaEntidad_CSV/Censo2017_Manzanas_write.csv") %>% 
  read_csv()

str(census_2017)
head(census_2017)

# Split 2017 Census data by Comunas

census_2017_ls <- census_2017 %>% 
  split(.$COMUNA)

census_LC <- census_2017_ls$`13128`

# Crime Data

crime <- file.path(vector_path, "delitos_por_cuadrantePolygon.shp") %>% 
  readOGR()

# Split crime SPDF into multiple SPDF groupby Comunas

crime_comuna_ls <- crime %>% 
  split(.$COMUNA)

crime_LC <- crime_comuna_ls$`LAS CONDES`
crime_PA <- crime_comuna_ls$`PUENTE ALTO`
crime_SM <- crime_comuna_ls$`SAN MIGUEL`

(tm_basemap(leaflet::providers$CartoDB.DarkMatte) +
    tm_shape(crime_PA) +
    tm_sf(col = "delitosv",
          title = "* per comunes, Puente Alto",
          palette = RColorBrewer::brewer.pal("delitosv", "Reds"),
          tm_scale_bar())
)

# Base price
base_price <- file.path(vector_path, "gran_stgoPolygon.shp") %>% 
  readOGR()

base_price_ls <- base_price %>% 
  split(.$NOM_COMUNA)

BasePrice_LC <- base_price_ls$`LAS CONDES`
BasePrice_PA <- base_price_ls$`PUENTE ALTO`
BasePrice_SM <- base_price_ls$`SAN MIGUEL`

#(tm_basemap(leaflet::providers$CartoDB.DarkMatte) +
#    tm_shape(BasePrice_SM) +
#    tm_sf(col = "valor_2",
#          title = "Base value per comunes, San Miguel",
#          palette = RColorBrewer::brewer.pal("valor_2", "Spectral"),
#          tm_scale_bar())
#)

writeOGR(BasePrice_LC, layer = "valor_2", dsn = file.path(vector_path, "BasePrice_LC.geojson"), 
         driver = "GeoJSON", overwrite_layer = TRUE)
writeOGR(BasePrice_SM, layer = "valor_2", dsn = file.path(vector_path, "BasePrice_SM.geojson"), 
         driver = "GeoJSON", overwrite_layer = TRUE)
writeOGR(BasePrice_PA, layer = "valor_2", dsn = file.path(vector_path, "BasePrice_PA.geojson"), 
         driver = "GeoJSON", overwrite_layer = TRUE)

# RASTER PRE-PROCESSING ----
# Unfortunately the AOI lies between 2 scenes
# Therefore, it is requried to load and mosaic 3 sets of 2 scenes

tif_20210717A_path <- file.path(raster_path, "20210717T143731_20210717T144707_T19HCC.tif")

tif_20210717A <- file.path(tif_20210717A_path) %>% 
  stack()

tif_20210717B_path <- file.path(raster_path, "20210717T143731_20210717T144707_T19HCD.tif")

tif_20210717B <- file.path(tif_20210717B_path) %>% 
  stack()


# Distribution of pixel

par(mfrow = c(1, 2))
boxplot(values(tif_20210717A))
boxplot(values(tif_20210717B))

#tif_20210717A_Spath <- file.path(raster_path, "Split_20210717A")
#tif_20210717B_Spath <- file.path(raster_path, "Split_20210717B")

#if (file.exists(tif_20210717A_Spath)){
#  print("Dir for split band exists")
#  setwd(tif_20210717A_Spath)
#  writeRaster(tif_20210717A, file.path(getwd(), "Split_20210717A"), format = "GTiff", 
#              bylayer = TRUE, overwrite = TRUE, suffix = nlayers())
#} else {
#  dir.create(tif_20210717A_Spath)
#  setwd(tif_20210717A_Spath)
#  writeRaster(tif_20210717A, file.path(getwd(), "Split_20210717A"), format = "GTiff", 
#              bylayer = TRUE, overwrite = TRUE, suffix = nlayers())
#}

#if (file.exists(tif_20210717B_Spath)){
#  print("Dir for split band exists")
#  setwd(tif_20210717B_Spath)
#  writeRaster(tif_20210717B, file.path(getwd(), "Split_20210717B"), format = "GTiff", 
#              bylayer = TRUE, overwrite = TRUE, suffix = nlayers())
#} else {
#  dir.create(tif_20210717B_Spath)
#  setwd(tif_20210717B_Spath)
#  writeRaster(tif_20210717B, file.path(getwd(), "Split_20210717B"), format = "GTiff", 
#              bylayer = TRUE, overwrite = TRUE, suffix = nlayers())
#}

#tif_20210717A_st <- file.path(tif_20210717A_Spath, "Split_20210717A.tif") %>% 
#  stack()

#tif_20210717B_st <- file.path(tif_20210717A_Spath, "Split_20210717A.tif") %>% 
#  stack()

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

plot(NDSI_20210717A, col = RColorBrewer::brewer.pal(3, "Blues"))
plot(NDSI_20210717B, col = RColorBrewer::brewer.pal(3, "Blues"))

writeRaster(NDSI_20210717A, file.path(raster_path, "NDSI_20210717A"), format="GTiff", overwrite=TRUE)
writeRaster(NDSI_20210717B, file.path(raster_path, "NDSI_20210717B"), format="GTiff", overwrite=TRUE)

#tif_20210717A <- brick(tif_20210717A)
#tif_20210717B <- brick(tif_20210717B)

# Apply snow mask
tif_20210717A_mask <- mask(tif_20210717A, NDSI_20210717A, inverse=TRUE, maskvalue=NA)
tif_20210717B_mask <- mask(tif_20210717B, NDSI_20210717B, inverse=TRUE, maskvalue=NA)

nlayers(tif_20210717B_mask)

# Plot to check masking
par(mfrow = c(1, 2))
plot(tif_20210717A_mask)
plot(tif_20210717B_mask)

writeRaster(tif_20210717A_mask, file.path(raster_path, "20210717A_mask"), format="GTiff", overwrite=TRUE)
writeRaster(tif_20210717B_mask, file.path(raster_path, "20210717B_mask"), format="GTiff", overwrite=TRUE)


# Clear Cache

rm(A_b11_SWIR, A_b8_NIR, B_b11_SWIR, B_b8_NIR, tif_20210717A_path, tif_20210717B_path,
   tif_20210717A, tif_20210717B)

#############

mask20210717A <- file.path(raster_path, "20210717A_mask.tif") %>% 
  stack()
mask20210717B <- file.path(raster_path, "20210717B_mask.tif") %>% 
  stack()

nlayers(mask20210717A)

ggRGB(mask20210717A, r=4, g=3, b=2, stretch = "hist")
ggRGB(mask20210717B, r=4, g=3, b=2, stretch = "hist")

# Mosaic --> then reproject

mosaic20210717AB <- mosaic(mask20210717A, mask20210717B, fun=mean)
mosaic20210717AB <- projectRaster(mosaic20210717AB, crs="+init=epsg:9184")

ggRGB(mosaic20210717AB, r=4, g=3, b=2, stretch="hist")

writeRaster(mosaic20210717AB, file.path(raster_path, "mosaic_20210717AB"), format="GTiff", overwrite=TRUE)

################################
#Raster pre-processing complete#
################################