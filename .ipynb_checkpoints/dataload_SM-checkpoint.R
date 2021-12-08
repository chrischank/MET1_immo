#######################################################
#HR replication of Hill & Scholz (2018) for San Miguel#
#Maintainer: Christopher Chan                         #
#Date: 2021-08-03                                     #
#Version: 0.0.1                                       #
#######################################################

pkgs <- c("rlang", "vctrs", "tidyverse", "rgdal", "RStoolbox", "sf", "rasterVis", "ggmap", "viridis", "osmdata",
          "tmap", "RColorBrewer", "ellipse", "corrplot", "dismo", "ggvoronoi",
          "mgcv", "maptools", "GWmodel")

for (i in pkgs){
  if (!require(i, character.only = TRUE)){
    install.packages(i, dependencies = TRUE)
    library(i, dependencies=TRUE)
  }
}

setwd("~/Dropbox/RS_Project/RSSTARTUP_repo/Product_Dev/immo_model")

figure_path <- file.path(getwd(), "Figures")
vector_path <- file.path(getwd(), "Vector")
raster_path <- file.path(getwd(), "Raster")

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

(tm_basemap(leaflet::providers$CartoDB.DarkMatte) +
    tm_shape(AOI_SM_les) +
    tm_sf(col = "leisure",
          title = "SM_leisure_type",
          palette = RColorBrewer::brewer.pal("leisure", "Pastel1"),
          tm_scale_bar())
)

# STREETS

AOI_SM_Street <- opq(bbox = AOI_san_miguel) %>%
  add_osm_feature(key = "highway", value = c("motorway", "trunk",
                                             "primary")) %>%
  osmdata_sf()

glimpse(AOI_SM_Street$osm_lines)

AOI_SM_Street <- AOI_SM_Street$osm_lines %>%
  as_Spatial()


AOI_SM_Street <- spTransform(AOI_SM_Street, CRS("+init=epsg:9184"))


################################
#Data pulling from OSM complete#
################################

# Write and read locally as SPDFs

# Boundaries
st_write(AOI_san_miguel, dsn = file.path(vector_path, "AOI_SM.geojson"),
         driver = "GeoJSON", append = FALSE)

# Streets
writeOGR(AOI_SM_Street, layer = "highway", dsn = file.path(vector_path, "AOI_SM_Street.geojson"),
         driver = "GeoJSON", overwrite_layer = TRUE)

# Why not node?

AOI_SM_StreetC <- st_read(file.path(vector_path, "AOI_SM_Street.geojson")) %>%
  st_centroid()

# Converts Leisure, Amenities, and Residentials polygons to points
# Take random sample per base_price polygons

AOI_SM_amyPoints <- AOI_SM_amy %>%
  st_as_sf() %>%
  st_cast("POINT")
AOI_SM_lesPoints <- AOI_SM_les %>%
  st_as_sf() %>%
  st_cast("POINT")
AOI_SM_resPoints <- AOI_SM_res %>%
  st_as_sf() %>%
  st_cast("POINT")

# Leisure, Amenities, and Residentials polygons and points

st_write(AOI_SM_amy, dsn = file.path(vector_path, "AOI_SM_amy.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_SM_amyPoints, dsn = file.path(vector_path, "AOI_SM_amyPoints.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_SM_les, dsn = file.path(vector_path, "AOI_SM_les.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_SM_lesPoints, dsn = file.path(vector_path, "AOI_SM_lesPoints.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_SM_res, dsn = file.path(vector_path, "AOI_SM_res.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_SM_resPoints, dsn = file.path(vector_path, "AOI_SM_resPoints.geojson"),
         driver = "GeoJSON", append = FALSE)


st_write(AOI_SM_StreetC, dsn = file.path(vector_path, "AOI_SM_StreetC.geojson"),
         driver = "GeoJSON", append = FALSE)

# Leisure, Amenities, and Residentials points

AOI_san_miguel <- readOGR(file.path(vector_path, "AOI_SM.geojson"))

AOI_SM_Street <- readOGR(file.path(vector_path, "AOI_SM_Street.geojson"))
AOI_SM_StreetC <- readOGR(file.path(vector_path, "AOI_SM_StreetC.geojson"))

AOI_SM_amy <- readOGR(file.path(vector_path, "AOI_SM_amy.geojson"))
AOI_SM_amyPoints <- readOGR(file.path(vector_path, "AOI_SM_amyPoints.geojson"))
AOI_SM_les <- readOGR(file.path(vector_path, "AOI_SM_les.geojson"))
AOI_SM_lesPoints <- readOGR(file.path(vector_path, "AOI_SM_lesPoints.geojson"))
AOI_SM_res <- readOGR(file.path(vector_path, "AOI_SM_res.geojson"))
AOI_SM_resPoints <- readOGR(file.path(vector_path, "AOI_SM_resPoints.geojson"))

# Pre-processing for streets, calculate residential euclidean distance to streets

par(mfrow = c(1, 2))
plot(AOI_SM_res)
plot(AOI_SM_Street)
dev.off()


# Pre-processing for Census 2017, Crime, Base price

# Since component of crime for different Comunas will be different,
# A corrplot will be ran and the highest 3 types of crime will be selected for model
# I.e. each Comuna (L.C., S.M., P.A.) shall have their own set of 3 highly correlated crime

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
dev.off()

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

mosaic20210717AB <- stack(file.path(raster_path, "mosaic_20210717AB.tif"))

################################
#Raster pre-processing complete#
################################

# NDVI are calculated for selected leisure space

green_les <- c("golf_course", "nature_reserve", "park", "garden")

AOI_NDVI <- RStoolbox::spectralIndices(mosaic20210717AB, red = 4, nir = 8, indices = "NDVI")

# Crop out NDVI of selected green leisure polygons

if (AOI_SM_les$leisure %in% green_les == TRUE){
  NDVI_SMles <- mask(AOI_NDVI, AOI_SM_les)
}

plot(NDVI_SMles, col = rev(terrain.colors(10)), main = "NDVI of San Miguel leisure space")
png(file.path(figure_path, "NDVI_AOI.png"), width = 10, height = 8, units = "in", res = 300)