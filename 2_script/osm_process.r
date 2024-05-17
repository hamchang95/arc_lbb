#--Load all features
source(here("2_script", "osm_features.r"))

########### SET UP ##########
#--Flatten list
place_list <- do.call(c, do.call(c, place_list_fin))
names(place_list) <- str_replace_all(names(place_list), fixed("."), "_")

#--Import park
park <- st_read(here("3_output", "place", "park.shp"))
place_list[["park"]] <- park

#--Remove empty list
place_list <- Filter(function(x) nrow(x) > 0, place_list)

#--Import Barnet shapefile
bnt_shp <- sf::st_read(here("1_data", "9_geo", "bnt_lad.json"), crs = 4326) |>
  st_make_valid()

########## PROCESS ########
#--Select only the features that intersect the Barnet's boundary
poi_bnt_check <- map(place_list, ~st_intersects(.x, bnt_shp, sparse = FALSE))
poi_bnt_check <- map(poi_bnt_check, ~which(.x == TRUE))

poi_bnt <- map2(.x = place_list, .y = poi_bnt_check, ~.x[.y,])
poi_bnt <- Filter(function(x) nrow(x) > 0, poi_bnt)

#--Divide features into polygons / points to get centroid for polygons
#---Check if features are polygon / point
poly_check <- map(poi_bnt, ~grepl("POLYGON", st_geometry_type(.x)))
point_check <- map(poi_bnt, ~grepl("POINT", st_geometry_type(.x)))

#---Filter features by geometry type
poi_bnt_poly <- map2(.x = poi_bnt, .y = poly_check, ~.x[.y,])
poi_bnt_pt <-  map2(.x = poi_bnt, .y = point_check, ~.x[.y,])

#---Remove empty list
poi_bnt_poly <- Filter(function(x) nrow(x) > 0, poi_bnt_poly)
poi_bnt_pt <- Filter(function(x) nrow(x) > 0, poi_bnt_pt)

poi_bnt_poly <- map(poi_bnt_poly, ~st_centroid(.x))
poi_bnt_fin <- c(poi_bnt_poly, poi_bnt_pt)

####### CLEAN UP #####
rm(list = c('poly_check', 'point_check', 'poi_bnt_poly', 'poi_bnt_pt', 'poi_bnt_check', 'i', 'j', 'place_list_fin', 'place_list', 'park'))