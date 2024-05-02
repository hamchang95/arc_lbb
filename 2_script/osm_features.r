########## SET UP ##########
#--Clear enviornment
rm(list = ls())

#--Set longer timeout
options(timeout = 120)

#--Install / load packages
pacman::p_load(sf, httr, jsonlite, here, tmap, osmdata, tidyverse, data.table, rmapshaper, xml2, rvest, mapview, rio)

#--Get Barnet polygon
bnt_bb <- getbb("London Borough of Barnet", format_out = "polygon")

#--Get Barnet boudnary
    # For easy lookup on boundary level: https://osm-boundaries.com/Map
bnt_bdry <- opq(bnt_bb) |> 
    add_osm_feature(key = "admin_level",value = 8) |>
    osmdata_sf()

#--Check the output 
ggplot(bnt_bdry$osm_multipolygons) +
    geom_sf()
    # it looks alright but it also returns other contingent boroughs
    # this is probably there will be some points or lines on the intersection 
    # will first download attributes data and filter out later on

#--Get list of features with values on OSM
dict <- import_list(here("0_ref", "places.xlsx")) 
dict <- dict[-c(1,2)]
dict <-map(dict, 
    ~.x |>
        filter(Use == 1) |>
        select(Value)
)
names(dict)

########### DOWNLOAD ###########
#--Define downloading function
get_features <- function(key, value) {
    data <- opq(bnt_bb) |> 
        add_osm_feature(key = key, value = value) |> 
        osmdata_sf()

    return(data)
}

#--Amenity
amenity_list <- vector("list", length(dict[[1]][["Value"]]))
amenity_list <- map(seq_along(dict[[1]][["Value"]]), function(i) {
  get_features("amenity", dict[[1]][["Value"]][[i]])
})
names(amenity_list) <- dict[[1]][["Value"]]


#--Public transport
public_transport_list <- vector("list", length(dict[[2]][["Value"]]))
public_transport_list <- map(seq_along(dict[[2]][["Value"]]), function(i) {
  get_features("public_transport", dict[[2]][["Value"]][[i]])
})
names(public_transport_list) <- dict[[2]][["Value"]]

#--Building
building_list <- vector("list", length(dict[[3]][["Value"]]))
building_list <- map(seq_along(dict[[3]][["Value"]]), function(i) {
  get_features("building", dict[[3]][["Value"]][[i]])
})
names(building_list) <- dict[[3]][["Value"]]

######### PLOT ###############
ggplot() + 
geom_sf(data = bnt_bdry[["osm_multipolygons"]])+
geom_sf(data = building_list[["hotel"]][["osm_points"]])+
theme_minimal()
