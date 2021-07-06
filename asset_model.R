###########################################################################
#Asset price exploration and prediction on 3 communes of Santiago de Chile#
#Maintainer: Christopher Chan                                             #
#Date: 2021-06-30                                                         #
#Version: 0.0.3                                                           #
###########################################################################

setwd("C:/Users/Chris/Dropbox/EAGLE_Assessments/MET1_Model/MET1_immo/")

pkgs <- c("tidyverse", "rgdal", "RStoolbox", "sf", "rasterVis", "ggmap", "viridis", "osmdata",
          "tmap", "RColorBrewer")

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

AOI_Santiago <- osmdata::getbb("Provincia de Santiago", featuretype = "boundaries",
                               format_out = "sf_polygon") %>%
  st_transform(AOI_Santiago, crs = 9184)

st_write(AOI_Santiago, dsn = file.path(vector_path, "AOI_Santiago.geojson"),
         driver = "GeoJSON", append = FALSE)

AOI_Santiago <- readOGR(file.path(vector_path, "AOI_Santiago.geojson"))

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

(tm_basemap(leaflet::providers$CartoDB.DarkMatter) +
    tm_shape(AOI_LC_res) +
    tm_sf(col = "building",
          title = "LC_residential_type",
          palette = RColorBrewer::brewer.pal("building", "Accent"),
          tm_scale_bar())
)

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

(tm_basemap(leaflet::providers$CartoDB.DarkMatter) +
    tm_shape(AOI_LC_amy) +
    tm_sf(col = "amenity",
          title = "LC_amenity_type",
          palette = RColorBrewer::brewer.pal("amenity", "Paired"),
          tm_scale_bar())
)


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

(tm_basemap(leaflet::providers$CartoDB.DarkMatter) +
    tm_shape(AOI_LC_les) +
    tm_sf(col = "leisure",
          title = "LC_leisure_type",
          palette = RColorBrewer::brewer.pal("leisure", "Pastel1"),
          tm_scale_bar())
)

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

(tm_basemap(leaflet::providers$CartoDB.DarkMatter) + # add Stamen Terrain basemap
    tm_shape(AOI_SM_res) + # add the sf
    tm_sf(col = "building", # colour by green space type
          title = "SM_residential_type", # no legend title
          palette = RColorBrewer::brewer.pal("building", "Accent"), # custom fill colours
          tm_scale_bar()) # add scale bar
)

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

(tm_basemap(leaflet::providers$CartoDB.DarkMatte) +
    tm_shape(AOI_SM_amy) +
    tm_sf(col = "amenity",
          title = "SM_amenity_type",
          palette = RColorBrewer::brewer.pal("amenity", "Paired"),
          tm_scale_bar())
)

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

(tm_basemap(leaflet::providers$CartoDB.DarkMatte) + # add Stamen Terrain basemap
    tm_shape(AOI_PA_res) + # add the sf
    tm_sf(col = "building", # colour by green space type
          title = "PA_residential_type", # no legend title
          palette = RColorBrewer::brewer.pal("building", "Accent"), # custom fill colours
          tm_scale_bar()) # add scale bar
)

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

(tm_basemap(leaflet::providers$CartoDB.DarkMatte) +
    tm_shape(AOI_PA_amy) +
    tm_sf(col = "amenity",
          title = "PA_amenity_type",
          palette = RColorBrewer::brewer.pal("amenity", "Paired"),
          tm_scale_bar())
)

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

(tm_basemap(leaflet::providers$CartoDB.DarkMatte) +
    tm_shape(AOI_PA_les) +
    tm_sf(col = "leisure",
          title = "PA_leisure_type",
          palette = RColorBrewer::brewer.pal("amenity", "Paired"),
          tm_scale_bar())
)

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

# Raster pre-processing ----
L2A_20210702 <- file.path(raster_path, "MASK_S2B_MSIL2A_20210702T143729_N0301_R096_T19HCD_20210702T184552",
                          "") %>% 
  raster()

