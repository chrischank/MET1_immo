###########################################################################
#Asset price exploration and prediction on 3 communes of Santiago de Chile#
#Maintainer: Christopher Chan                                             #
#Date: 2021-08-03                                                         #
#Version: 0.0.6                                                           #
###########################################################################

pkgs <- c("rlang", "vctrs", "tidyverse", "rgdal", "RStoolbox", "sf", "rasterVis", "ggmap", "viridis", "osmdata",
          "tmap", "RColorBrewer", "reticulate", "ellipse", "corrplot", "dismo", "ggvoronoi",
          "mgcv", "maptools")

for (i in pkgs){
  if (!require(i, character.only = TRUE)){
    install.packages(i, dependencies = TRUE)
    library(i, dependencies=TRUE)
  }
}

setwd("~/Dropbox/EAGLE_Assessments/MET1_Model/MET1_immo")

figure_path <- file.path(getwd(), "Figures")
vector_path <- file.path(getwd(), "Vector")
raster_path <- file.path(getwd(), "Raster")

# VECTOR PRE-PROCESSING ----

# AOI boundaries from OSM

########################
#BBox Santiago de Chile#
########################

AOI_Santiago <- readOGR(file.path(vector_path, "AOI_Santiago.geojson"))
AOI_Santiago <- spTransform(AOI_Santiago, CRS("+init=epsg:9184"))

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

AOI_Street <- opq(bbox = AOI_Santiago) %>%
  add_osm_feature(key = "highway", value = c("motorway", "trunk",
                                             "primary")) %>%
  osmdata_sf()

glimpse(AOI_LC_Street$osm_lines)


AOI_LC_Street <- opq(bbox = AOI_las_condes) %>%
  add_osm_feature(key = "highway", value = c("motorway", "trunk",
                                             "primary")) %>%
  osmdata_sf()

glimpse(AOI_LC_Street$osm_lines)

AOI_LC_Street <- AOI_LC_Street$osm_lines %>%
  as_Spatial()


AOI_LC_Street <- spTransform(AOI_LC_Street, CRS("+init=epsg:9184"))

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


AOI_SM_Street <- spTransform(AOI_SM_Street, CRS("+init=epsg:9184"))


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


AOI_PA_Street <- spTransform(AOI_PA_Street, CRS("+init=epsg:9184"))

################################
#Data pulling from OSM complete#
################################

# Write and read locally as SPDFs

# Boundaries
st_write(AOI_las_condes, dsn = file.path(vector_path, "AOI_LC.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_san_miguel, dsn = file.path(vector_path, "AOI_SM.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_puente_alto, dsn = file.path(vector_path, "AOI_PA.geojson"),
         driver = "GeoJSON", append = FALSE)

# Streets
writeOGR(AOI_LC_Street, layer = "highway", dsn = file.path(vector_path, "AOI_LC_Street.geojson"),
         driver = "GeoJSON", overwrite_layer = TRUE)
writeOGR(AOI_SM_Street, layer = "highway", dsn = file.path(vector_path, "AOI_SM_Street.geojson"),
         driver = "GeoJSON", overwrite_layer = TRUE)
writeOGR(AOI_PA_Street, layer = "highway", dsn = file.path(vector_path, "AOI_PA_Street.geojson"),
         driver = "GeoJSON", overwrite_layer = TRUE)

# Why not node?

AOI_LC_StreetC <- st_read(file.path(vector_path, "AOI_LC_Street.geojson")) %>%
  st_centroid()
AOI_SM_StreetC <- st_read(file.path(vector_path, "AOI_SM_Street.geojson")) %>%
  st_centroid()
AOI_PA_StreetC <- st_read(file.path(vector_path, "AOI_PA_Street.geojson")) %>%
  st_centroid()

# Converts Leisure, Amenities, and Residentials polygons to points
# Take random sample per base_price polygons

AOI_LC_amyPoints <- AOI_LC_amy %>%
  st_as_sf() %>%
  st_cast("POINT")
AOI_LC_lesPoints <- AOI_LC_les %>%
  st_as_sf() %>%
  st_cast("POINT")
AOI_LC_resPoints <- AOI_LC_res %>%
  st_as_sf() %>%
  st_cast("POINT")

AOI_PA_amyPoints <- AOI_PA_amy %>%
  st_as_sf() %>%
  st_cast("POINT")
AOI_PA_lesPoints <- AOI_PA_les %>%
  st_as_sf() %>%
  st_cast("POINT")
AOI_PA_resPoints <- AOI_PA_res %>%
  st_as_sf() %>%
  st_cast("POINT")

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

st_write(AOI_LC_amy, dsn = file.path(vector_path, "AOI_LC_amy.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_LC_amyPoints, dsn = file.path(vector_path, "AOI_LC_amyPoints.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_LC_les, dsn = file.path(vector_path, "AOI_LC_les.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_LC_lesPoints, dsn = file.path(vector_path, "AOI_LC_lesPoints.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_LC_res, dsn = file.path(vector_path, "AOI_LC_res.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_LC_resPoints, dsn = file.path(vector_path, "AOI_LC_resPoints.geojson"),
         driver = "GeoJSON", append = FALSE)

st_write(AOI_PA_amy, dsn = file.path(vector_path, "AOI_PA_amy.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_PA_amyPoints, dsn = file.path(vector_path, "AOI_PA_amyPoints.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_PA_les, dsn = file.path(vector_path, "AOI_PA_les.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_PA_lesPoints, dsn = file.path(vector_path, "AOI_PA_lesPoints.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_PA_res, dsn = file.path(vector_path, "AOI_PA_res.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_PA_resPoints, dsn = file.path(vector_path, "AOI_PA_resPoints.geojson"),
         driver = "GeoJSON", append = FALSE)

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


st_write(AOI_LC_StreetC, dsn = file.path(vector_path, "AOI_LC_StreetC.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_SM_StreetC, dsn = file.path(vector_path, "AOI_SM_StreetC.geojson"),
         driver = "GeoJSON", append = FALSE)
st_write(AOI_PA_StreetC, dsn = file.path(vector_path, "AOI_PA_StreetC.geojson"),
         driver = "GeoJSON", append = FALSE)

# Leisure, Amenities, and Residentials points

AOI_las_condes <- readOGR(file.path(vector_path, "AOI_LC.geojson"))
AOI_san_miguel <- readOGR(file.path(vector_path, "AOI_SM.geojson"))
AOI_puente_alto <- readOGR(file.path(vector_path, "AOI_PA.geojson"))

AOI_LC_Street <- readOGR(file.path(vector_path, "AOI_LC_Street.geojson"))
AOI_SM_Street <- readOGR(file.path(vector_path, "AOI_SM_Street.geojson"))
AOI_PA_Street <- readOGR(file.path(vector_path, "AOI_PA_Street.geojson"))
AOI_LC_StreetC <- readOGR(file.path(vector_path, "AOI_LC_StreetC.geojson"))
AOI_SM_StreetC <- readOGR(file.path(vector_path, "AOI_SM_StreetC.geojson"))
AOI_PA_StreetC <- readOGR(file.path(vector_path, "AOI_PA_StreetC.geojson"))

AOI_LC_amy <- readOGR(file.path(vector_path, "AOI_LC_amy.geojson"))
AOI_LC_amyPoints <- readOGR(file.path(vector_path, "AOI_LC_amyPoints.geojson"))
AOI_LC_les <- readOGR(file.path(vector_path, "AOI_LC_les.geojson"))
AOI_LC_lesPoints <- readOGR(file.path(vector_path, "AOI_LC_lesPoints.geojson"))
AOI_LC_res <- readOGR(file.path(vector_path, "AOI_LC_res.geojson"))
AOI_LC_resPoints <- readOGR(file.path(vector_path, "AOI_LC_resPoints.geojson"))

AOI_SM_amy <- readOGR(file.path(vector_path, "AOI_SM_amy.geojson"))
AOI_SM_amyPoints <- readOGR(file.path(vector_path, "AOI_SM_amyPoints.geojson"))
AOI_SM_les <- readOGR(file.path(vector_path, "AOI_SM_les.geojson"))
AOI_SM_lesPoints <- readOGR(file.path(vector_path, "AOI_SM_lesPoints.geojson"))
AOI_SM_res <- readOGR(file.path(vector_path, "AOI_SM_res.geojson"))
AOI_SM_resPoints <- readOGR(file.path(vector_path, "AOI_SM_resPoints.geojson"))

AOI_PA_amy <- readOGR(file.path(vector_path, "AOI_PA_amy.geojson"))
AOI_PA_amyPoints <- readOGR(file.path(vector_path, "AOI_PA_amyPoints.geojson"))
AOI_PA_les <- readOGR(file.path(vector_path, "AOI_PA_les.geojson"))
AOI_PA_lesPoints <- readOGR(file.path(vector_path, "AOI_PA_lesPoints.geojson"))
AOI_PA_res <- readOGR(file.path(vector_path, "AOI_PA_res.geojson"))
AOI_PA_resPoints <- readOGR(file.path(vector_path, "AOI_PA_resPoints.geojson"))

# Pre-processing for streets, calculate residential euclidean distance to streets

par(mfrow = c(1, 2))
plot(AOI_LC_res)
plot(AOI_LC_Street)
dev.off()

# Pre-processing for Census 2017, Crime, Base price

# Since component of crime for different Comunas will be different,
# A corrplot will be ran and the highest 3 types of crime will be selected for model
# I.e. each Comuna (L.C., S.M., P.A.) shall have their own set of 3 highly correlated crime

# Census_2017 Data

census_2017 <- file.path(vector_path, "r13", "R13", ) %>%
  read_csv(col_names = TRUE)

glimpse(census_2017)
head(census_2017)

# Split 2017 Census data by Comunas

census_2017_ls <- census_2017 %>%
  split(.$COMUNA)

census_LC <- census_2017_ls$`15108`
census_SM <- census_2017_ls$`16106`
census_PA <- census_2017_ls$`16301`

census_SM

# Crime Data

crime <- file.path(vector_path, "delitos_por_cuadrantePolygon.shp") %>%
  readOGR()

# Split crime SPDF into multiple SPDF groupby Comunas

crime_comuna_ls <- crime %>%
  split(.$COMUNA)

crime_LC <- crime_comuna_ls$`LAS CONDES`
crime_PA <- crime_comuna_ls$`PUENTE ALTO`
crime_SM <- crime_comuna_ls$`SAN MIGUEL`

#(tm_basemap(leaflet::providers$CartoDB.DarkMatte) +
#    tm_shape(crime_PA) +
#    tm_sf(col = "delitosv",
#          title = "* per comunes, Puente Alto",
#          palette = RColorBrewer::brewer.pal("delitosv", "Reds"),
#          tm_scale_bar())
#)

# Since it is unclear what delitosv contain
# Using corrplot to select the crime types most correlated (> 0.7) to delitosv

# Crime for Las Condes
crimeLC_df <- as_tibble(crime_LC)

crimeLC_df <- crimeLC_df %>%
  dplyr::select(delitosv, homicidi, robovio, robointim, robosopr, lesiones, violacion,
                delproprie, robovehicu, robovehint, robohabita, robonohabi, otrosrobos,
                hurtos) %>%
  na.omit()

delitosLC_proxy <- cor(crimeLC_df, method = "pearson")
ord <- order(delitosLC_proxy[1, ])
data_ord <- delitosLC_proxy[ord, ord]
(cor_crimeLC <- corrplot::corrplot.mixed(data_ord, upper = "number", lower = "ellipse",
                                         order = "alphabet", tl.pos = "lt", tl.col = "black"))

# Find attributes that are highly corrected (ideally > 0.7)
# For Las Condes, highest crime correlation to delitosv are:
# 1. robosopr (r = 0.98)
# 2. robovio (r = 0.97)
# 3. robointim (r = 0.97)

# Crime for Puente Alto

crimePA_df <- as_tibble(crime_PA)

crimePA_df <- crimePA_df %>%
  dplyr::select(delitosv, homicidi, robovio, robointim, robosopr, lesiones, violacion,
                delproprie, robovehicu, robovehint, robohabita, robonohabi, otrosrobos,
                hurtos) %>%
  na.omit()

delitosPA_proxy <- cor(crimePA_df, method = "pearson")
ord <- order(delitosPA_proxy[1, ])
data_ord <- delitosPA_proxy[ord, ord]
(cor_crimePA <- corrplot::corrplot.mixed(data_ord, upper = "number", lower = "ellipse",
                                         order = "alphabet", tl.pos = "lt", tl.col = "black"))

# Find attributes that are highly corrected (ideally > 0.7)
# For Puente Alto, highest crime correlation to delitosv are:
# 1. robovio (r = 0.95)
# 2. lesiones (r = 0.93)
# 3. robointim (r = 0.91)

# Crime for San Miguel

crimeSM_df <- as_tibble(crime_SM)

crimeSM_df <- crimeSM_df %>%
  dplyr::select(delitosv, homicidi, robovio, robointim, robosopr, lesiones, violacion,
                delproprie, robovehicu, robovehint, robohabita, robonohabi, otrosrobos,
                hurtos) %>%
  na.omit()

delitosSM_proxy <- cor(crimeSM_df, method = "pearson")
ord <- order(delitosSM_proxy[1, ])
data_ord <- delitosSM_proxy[ord, ord]
(cor_crimeSM <- corrplot::corrplot.mixed(data_ord, upper = "number", lower = "ellipse",
                                         order = "alphabet", tl.pos = "lt", tl.col = "black"))

# Find attributes that are highly corrected (ideally > 0.7)
# For San Miguel, highest crime correlation to delitosv are:
# 1. robovehint (r = 0.97)
# 2. delproprie (r = 0.95)
# 3. hurtos (r = 0.94)

# Base price
base_price <- file.path(vector_path, "gran_stgoPolygon.shp") %>%
  readOGR()

base_price <- spTransform(base_price, CRS("+init=epsg:9184"))

base_price_ls <- base_price %>%
  split(.$NOM_COMUNA)

BasePrice_LC <- base_price_ls$`LAS CONDES`
BasePrice_PA <- base_price_ls$`PUENTE ALTO`
BasePrice_SM <- base_price_ls$`SAN MIGUEL`

(tm_basemap(leaflet::providers$CartoDB.DarkMatte) +
    tm_shape(BasePrice_LC) +
    tm_sf(col = "valor_2",
          title = "Base value per comunes, Las Condes",
          palette = RColorBrewer::brewer.pal("valor_2", "Spectral"),
          tm_scale_bar())
)

writeOGR(BasePrice_LC, layer = "valor_2", dsn = file.path(vector_path, "BasePrice_LC.geojson"),
         driver = "GeoJSON", overwrite_layer = TRUE)
writeOGR(BasePrice_SM, layer = "valor_2", dsn = file.path(vector_path, "BasePrice_SM.geojson"),
         driver = "GeoJSON", overwrite_layer = TRUE)
writeOGR(BasePrice_PA, layer = "valor_2", dsn = file.path(vector_path, "BasePrice_PA.geojson"),
         driver = "GeoJSON", overwrite_layer = TRUE)

# Read locally
BasePrice_LC <- readOGR(file.path(vector_path, "BasePrice_LC.geojson"))
BasePrice_SM <- readOGR(file.path(vector_path, "BasePrice_SM.geojson"))
BasePrice_PA <- readOGR(file.path(vector_path, "BasePrice_PA.geojson"))

# Combine dataframe to explore data
BasePrice_total <- bind_rows(BasePrice_LC@data, BasePrice_PA@data, BasePrice_SM@data)

glimpse(BasePrice_total)

# Plot boxplot for base price in the 3 Comunas

(BPrice_hist <- ggplot(BasePrice_total, aes(x=NOM_COMUNA, y=valor_2,
                                            fill=NOM_COMUNA))+
    geom_boxplot()+
    viridis::scale_fill_viridis(discrete = TRUE, alpha=0.6)+
    geom_jitter(color="black", size=0.4, alpha=0.9)+
    xlab("Comunas")+
    ylab("Base Price (UF)")+
    ggtitle("Boxplot of base price of the 3 selected Comunas of Santiago de Chile"))

ggsave(file.path(figure_path, "basepriceUF_box.png"), width = 7, height = 5, dpi = 300)

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

AOI_LC_les$leisure

AOI_NDVI <- RStoolbox::spectralIndices(mosaic20210717AB, red = 4, nir = 8, indices = "NDVI")

# Crop out NDVI of selected green leisure polygons

if (AOI_LC_les$leisure %in% green_les == TRUE){
  NDVI_LCles <- mask(AOI_NDVI, AOI_LC_les)
}

if (AOI_PA_les$leisure %in% green_les == TRUE){
  NDVI_PAles <- mask(AOI_NDVI, AOI_PA_les)
}

if (AOI_SM_les$leisure %in% green_les == TRUE){
  NDVI_SMles <- mask(AOI_NDVI, AOI_SM_les)
}

par(mfrow = c(1, 3))
plot(NDVI_SMles, col = rev(terrain.colors(10)), main = "NDVI of San Miguel leisure space")
plot(NDVI_PAles, col = rev(terrain.colors(10)), main = "NDVI of Puente Alto leisure space")
plot(NDVI_LCles, col = rev(terrain.colors(10)), main = "NDVI of Las Condes leisure space")

png(file.path(figure_path, "NDVI_AOI.png"), width = 10, height = 8, units = "in", res = 300)

# Constructing Large DFs for the 3 Comunas


# Proxy selection and modelling ----

##############################################################################
#Geographically Weighted Hedonic Regression Model as a function of base price#
##############################################################################
# (Tsutsumi & Seya, 2009)
# (Hill, 2011)
# (Hill & Scholz, 2018)
# (Brimble et al., 2020)

# A hedonic model regresses the price of a product on a vector of characteristics (whose
# prices are not independently observed). The hedonic equation is a reduced form equation
# that is determined by the interaction of supply and demand. (Hill, 2011)


##############################################
#DONE IN QGIS DUE TO LIMITED MEMORY ISSUE    #
#SAMPLING AND DISTANCE ARE CALCULATED IN QGIS#
##############################################

# Remove cache for Points data before loading clipped samples
rm(AOI_LC_amyPoints, AOI_LC_lesPoints, AOI_LC_resPoints,
   AOI_SM_amyPoints, AOI_SM_lesPoints, AOI_SM_resPoints,
   AOI_PA_amyPoints, AOI_PA_lesPoints, AOI_PA_resPoints,
   i)

# readOGR does not work well with multipoints
LC_resPSamp <- st_read(file.path(vector_path, "Samples", "LC_resPSamp.geojson")) %>%
  st_transform(crs = 9184)

LC_lesPSamp <- st_read(file.path(vector_path, "Samples", "LC_lesPSamp.geojson")) %>%
  st_transform(crs = 9184)

LC_amyPSamp <- st_read(file.path(vector_path, "Samples", "LC_amyPSamp.geojson")) %>%
  st_transform(crs = 9184)

LC_streetCSamp <- st_read(file.path(vector_path, "Samples", "LC_streetCSamp.geojson")) %>%
  st_transform(crs = 9184)


BasePrice_LC <- st_as_sf(BasePrice_LC) %>%
  st_transform(crs = 9184)

# Loop through each Base_Price polygons to calculate distance within

# Distance calculation per polygon Loops for Las Condes


# Create Dense Logical Matrix (sgbp) for each point to Base Price polygons
#mat_LClesBase = st_within(LC_lesPSamp[], BasePrice_LC[], sparse = TRUE)
#mat_LCresBase = st_within(LC_resPSamp[], BasePrice_LC[], sparse = TRUE)
#mat_LCamyBase = st_within(LC_amyPSamp[], BasePrice_LC[], sparse = TRUE)

# Distance calculation per polygon Loops for San Miguel

#################

for (i in BasePrice_LC[i, 8]){
beginCluster()

DIST_LCresles = st_multilinestring()
DIST_LCresles <-  if (st_within(LC_lesPSamp[, 2], i, sparse = FALSE) == TRUE &
                      st_within(LC_resPSamp[, 2], i, sparse = FALSE) == TRUE){
  st_distance(LC_lesPSamp, LC_resPSamp) %>%
    append(DIST_LCresles)
}

DIST_LCresamy = st_multilinestring()
DIST_LCresamy <-  if (st_within(LC_amyPSamp[, 2], i, sparse = FALSE) == TRUE &
                      st_within(LC_resPSamp[, 2], i, sparse = FALSE) == TRUE){
  st_distance(LC_amyPSamp, LC_resPSamp) %>%
    append(DIST_LCresamy)
}

DIST_LCresStreet = st_multilinestring()
DIST_LCresStreet <- if (st_within(LC_streetCSamp[, 2], i, sparse = FALSE) == TRUE &
                        st_within(LC_resPSamp[, 2], i, sparse = FALSE) == TRUE){
  st_distance(LC_streetCSamp, LC_resPSamp) %>%
    append(DIST_LCresStreet)
}
endCluster()
}



################

for (i in BasePrice_LC[]){
  for (j in LC_resPSamp[]){
    for (k in LC_lesPSamp[]){
      for (l in LC_amyPSamp[]){
        for (m in LC_streetCSamp[]){
          beginCluster()

        DIST_LCresles = st_multilinestring() %>%
          st_transform(crs = 9184)

        if (st_within(j, i, sparse = FALSE) == TRUE &
            st_within(k, i, sparse = FALSE) == TRUE){
          st_distance(k, j) %>%
            append(DIST_LCresles)

          endCluster()
          }
        }
      }
    }
  }
}


#############
# Try python#
#############

###############

# Parcel characteristic breakdown into 1. Structural 2. Locational 3. Neighbourhood-level
# 1. Structural: res$building
# 2. Physical: NDVIles
# 3. distance 2 amyPoints; distance 2 lesPoints


###########################
#Proxy selection with mRMR#
###########################

LC_LargeDF <-

# mRMR = minimum Redundancy Maximum Relevance
# using Ordinary Least Square

# Split dataset into training and test dataset
set.seed(2)
fold <-

# GAM

LC_gam <- gam(BasePrice_LC ~ family = gaussian(), )
