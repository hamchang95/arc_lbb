########## SET UP ##########
#--Clear enviornment
rm(list = ls())

#--Options
options(timeout = 120)

#--Install / load packages
pacman::p_load(sf, httr, jsonlite, here, tmap, osmdata, tidyverse, utf8, data.table, rmapshaper, xml2, rvest, rio, tidygeocoder)

#--Import data
cctv_safety_raw <- rio::import(here("1_data", "cctv_list_1223.csv"))
cctv_traffic_raw <- rio::import(here("1_data", "cctv_traffic_list.csv"))

#--Tidy names
names(cctv_safety_raw) <- str_to_lower(names(cctv_safety_raw))
names(cctv_traffic_raw) <- str_to_lower(str_replace_all(names(cctv_traffic_raw), " ", "_"))

####### PROCESS #########
#--Inspect 
str(cctv_safety_raw)
    #location includes street name without postcode 

str(cctv_traffic_raw)
    #location_description includes street name and outcode

#--Geocode
cctv_safety <- cctv_safety_raw |> 
    mutate(across(where(is.character), ~utf8::utf8_encode(.x))) |>
    mutate(location = gsub("\\s*\\([^\\)]+\\)", "", location)) |>
    mutate(location = str_replace_all(location, fixed("Ave"), "Avenue")) |>
    mutate(cleaned_location =  gsub("(.+?\\b(?:\\S+\\s+){0,2})(street|road|way|lane|avenue)\\b.*", "\\1\\2", location, ignore.case = TRUE)) |>
    mutate(cleaned_location = ifelse(grepl("Northway Circus", cleaned_location), "Watford Way", cleaned_location)) |>
    tidygeocoder::geocode(
        address = cleaned_location, 
        method = "arcgis"
    )

cctv_safety_v2 <- cctv_safety |> 
    tidygeocoder::geocode(
        address = cleaned_location, 
        method = "osm"
    ) 
    
cctv_safety_v2 <- cctv_safety_v2|>
    mutate(lat = coalesce(`lat...6`, `lat...8`)) |> 
    mutate(long = coalesce(`long...7`, `long...9`)) |>
    mutate(lat = case_when(
        cleaned_location == "High Road" ~ `lat...6`,
        .default = lat
    )) |>
    mutate(long = case_when(
        cleaned_location == "High Road" ~ `long...7`,
        .default = long
    ))

names(cctv_safety_v2) <- paste0(names(cctv_safety_v2), "_osm")

cctv_safety_v3 <- cctv_safety |> 
    tidygeocoder::geocode(
        address = cleaned_location, 
        method = "google"
    ) 

cctv_safety_v3 <- cctv_safety_v3|>
    mutate(lat = coalesce(`lat...6`, `lat...8`)) |> 
    mutate(long = coalesce(`long...7`, `long...9`)) |>
    mutate(lat = case_when(
        cleaned_location == "High Road" ~ `lat...6`,
        .default = lat
    )) |>
    mutate(long = case_when(
        cleaned_location == "High Road" ~ `long...7`,
        .default = long
    )) |>
    mutate(lat = round(lat, 5), long = round(long, 5))

names(cctv_safety_v3) <- paste0(names(cctv_safety_v3), "_google")

cctv_safety_total <- cctv_safety |> 
    left_join(cctv_safety_v2[c("cleaned_location_osm", "lat_osm", "long_osm")], by=join_by("cleaned_location" == "cleaned_location_osm")) |>
    mutate(lat = case_when(
        is.na(lat) & !is.na(lat_osm) ~ lat_osm,
        .default = lat
    )) |>
    mutate(long = case_when(
        is.na(long) & !is.na(long_osm) ~ long_osm,
        .default = long
    ))|>
    mutate(lat = round(lat, 5), long = round(long, 5)) 
    drop(duplicated(.))
    

cctv_safety$lat |> is.na() |> sum()
cctv_safety_v2$lat_osm |> is.na() |> sum()
cctv_safety_v3$lat |> is.na() |> sum()
cctv_safety_total$lat |> is.na() |> sum()
table(cctv_safety_v3$lat == cctv_safety_total$lat)

Sys.setenv(GOOGLEGEOCODE_API_KEY = API_KEY)

cctv_traffic <- cctv_traffic_raw |> 
    separate(location_description, into = paste0("outcode_", letters[1:20]), remove = FALSE) |>
    mutate(across(paste0("outcode_", letters[1:20]), ~ifelse(grepl("(NW|HA|EN|N\\d)", .x), .x, NA))) |>
    select(-paste0("outcode_", letters[15:20])) 

lapply(cctv_traffic[,letters[1:20]], unique)

outcode_cols <- names(cctv_traffic)[grepl("outcode_", names(cctv_traffic))]

for (i in 1:(length(outcode_cols)-1)){
    cctv_traffic$outcode <- coalesce(cctv_traffic$outcode, cctv_traffic[[outcode_cols[i]]], cctv_traffic[[outcode_cols[i+1]]])
}

cctv_traffic <- cctv_traffic |> 
    select(-paste0("outcode_", letters[1:14])) |>
    relocate(outcode, .after = location_description)


API_KEY <- "AIzaSyCjxhelYNRXnXvyzdg9q79XIXiXMuqWB9A"

