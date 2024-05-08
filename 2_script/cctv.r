########## SET UP ##########
#--Clear enviornment
rm(list = ls())

#--Options
options(timeout = 120)
API_KEY <- "AIzaSyCjxhelYNRXnXvyzdg9q79XIXiXMuqWB9A"
Sys.setenv(GOOGLEGEOCODE_API_KEY = API_KEY)

#--Install / load packages
pacman::p_load(sf, httr, jsonlite, here, tmap, osmdata, tidyverse, utf8, data.table, rmapshaper, xml2, rvest, rio, tidygeocoder)

#--Import data
cctv_safety_raw <- rio::import(here("1_data", "cctv_list_1223.csv")) #cctvs installed for community safety
cctv_traffic_raw <- rio::import(here("1_data", "cctv_traffic_list.csv")) #cctvs on roads for traffic control

#--Tidy names
names(cctv_safety_raw) <- str_to_lower(names(cctv_safety_raw))
names(cctv_traffic_raw) <- str_to_lower(str_replace_all(names(cctv_traffic_raw), " ", "_"))

####### PROCESS #########
#--Inspect 
str(cctv_safety_raw)
    #location includes street name without postcode 

str(cctv_traffic_raw)
    #location_description includes street name and outcode

#--Get bounding box of Barnet
bb_lbb <- osmdata::getbb("London Barnet") |>
    as.data.frame()

#--Tidy up & geocode
#---Using ArcGIS geocoder
cctv_safety <- cctv_safety_raw |> 
    mutate(across(where(is.character), ~utf8::utf8_encode(.x))) |>
    mutate(location = gsub("\\s*\\([^\\)]+\\)", "", location)) |>
    mutate(location = str_replace_all(location, fixed("Ave"), "Avenue")) |>
    mutate(cleaned_location =  gsub("(.+?\\b(?:\\S+\\s+){0,2})(street|road|way|lane|avenue)\\b.*", "\\1\\2", location, ignore.case = TRUE)) |>
    mutate(cleaned_location = ifelse(grepl("Northway Circus", cleaned_location), "Watford Way", cleaned_location)) |>
    tidygeocoder::geocode(
        address = cleaned_location, 
        method = "arcgis"
    ) # Converts street into lat & long  

#---QC
cctv_safety <- cctv_safety |> 
    mutate(check_long = ifelse(long >= bb_lbb$min[1] & long <= bb_lbb$max[1], TRUE, FALSE)) |>
    mutate(check_lat = ifelse(lat >= bb_lbb$min[2] & long <= bb_lbb$max[2], TRUE, FALSE)) |>
    mutate(true_address = ifelse(check_lat == TRUE & check_long == TRUE, 1, 0)) 
    
cctv_safety |> 
    summarise(total = sum(true_address, na.rm = TRUE))
    # only 1 address is TRUE

#---Using OSM geocoder
cctv_safety_v2 <- cctv_safety |> 
    tidygeocoder::geocode(
        address = cleaned_location, 
        method = "osm"
    ) 
#----For OSM geocoder, two pairs of potential lat & long are returned
cctv_safety_v2 <- cctv_safety_v2|>
    mutate(lat = coalesce(`lat...6`, `lat...8`)) |> 
    mutate(long = coalesce(`long...7`, `long...9`)) 

#---QC
cctv_safety_v2 <- cctv_safety_v2 |> 
    mutate(check_long = ifelse(long >= bb_lbb$min[1] & long <= bb_lbb$max[1], TRUE, FALSE)) |>
    mutate(check_lat = ifelse(lat >= bb_lbb$min[2] & long <= bb_lbb$max[2], TRUE, FALSE)) |>
    mutate(true_address = ifelse(check_lat == TRUE & check_long == TRUE, 1, 0)) 
    
cctv_safety_v2 |> 
    summarise(total = sum(true_address, na.rm = TRUE))
    # only 1 address is TRUE

#---Using Google geocoder
cctv_safety_v3 <- cctv_safety |> 
    tidygeocoder::geocode(
        address = cleaned_location, 
        method = "google"
    ) 

cctv_safety_v3 <- cctv_safety_v3|>
    mutate(lat = coalesce(`lat...6`, `lat...11`)) |> 
    mutate(long = coalesce(`long...7`, `long...12`))

#---QC
cctv_safety_v3 <- cctv_safety_v3 |> 
    mutate(check_long = ifelse(long >= bb_lbb$min[1] & long <= bb_lbb$max[1], TRUE, FALSE)) |>
    mutate(check_lat = ifelse(lat >= bb_lbb$min[2] & long <= bb_lbb$max[2], TRUE, FALSE)) |>
    mutate(true_address = ifelse(check_lat == TRUE & check_long == TRUE, 1, 0)) 
    
cctv_safety_v3 |> 
    summarise(total = sum(true_address, na.rm = TRUE))
    # only 1 TRUE address

######### CONCLUSION ########
# Cannot geocode address column for CCTV cameras