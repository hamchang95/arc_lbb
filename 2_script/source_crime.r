########## SET UP ##########
#-Clear enviornment
rm(list = ls())

#-Install / load packages
pacman::p_load(sf, httr, jsonlite, here, tmap, osmdata, tidyverse, data.table, rmapshaper)

#-Path setting
path_home <- here()
path_data <- paste0(path_home, "/1_data")
path_output <- paste0(path_home, "/3_output")

### BOUNDARY ###
#-Import boundary 
lad_bdry <- sf::st_read(paste0(path_data, "/9_geo", "/bnt_lad.json")) 
st_crs(lad_bdry) <- 4326
plot(st_geometry(lad_bdry))

#-Convert boundary polygon to a set of coordinates with lat & lng
coords <- lad_bdry |> 
  st_coordinates() |> 
  as.data.frame() |>
  dplyr::select(X, Y)

plot(coords)

#-Format coordinates for the POST call
coords_list <- paste0(round(coords$Y,5),',',round(coords$X,5), collapse=':')

### API ###
#Refer to this page (https://data.police.uk/docs/method/crime-street/) for making POST calls
#-API paths
pth <- 'https://data.police.uk/api/crimes-street/all-crime' #for street-level crime data
pth_out <- 'https://data.police.uk/api/outcomes-at-location' #for outcome data
pth_stp <- 'https://data.police.uk/api/stops-street' #for stop-and-search records 

#-Dummy to check latest dates
dummy <- POST(url = pth, body = list(poly = coords_list)) |> content()
dummy[length(dummy)]
  #2024-02
latest_month <- dummy[[1]][["month"]]

#-Set periods of interest to pull  
yr <- rep(2018:2024, 12) |> sort()
mth <- rep(c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), length(yr)/12)
dt_list <- paste0(yr, "-", mth)

latest_dt <- lubridate::rollforward(ym(latest_month)) |> as.character()

########## PULL DATA ########
#-Define function to pull data
get_data <- function(url, poly, date){
  req <- list(); res <- list()
  
  for (i in seq_along(date)){
    req[[i]] <- httr::POST(url = url,body = list(poly = poly, date = date[i]))
    
    if(req[[i]]$status_code == 200){
      print("Request was successful")
      
      #--Get contents
      res[[i]] <- httr::content(req[[i]], simplifyDataFrame=TRUE, flatten=TRUE)
      
    } else if (req[[i]]$status_code %in% c(422, 405)) {
      print("Request failed due to an user's error")
    } else if (req[[i]]$status_code == 544){
      print("Request failed due to gateway timeout")
    }
  }
  
  #--Bind by rows
  res <- data.table::rbindlist(res)
  return(res)
}

#-Pull st-level crime data 
#crime <- get_data(url = pth, poly = coords_list, date = dt_list)
str(crime)
lapply(crime[,c(1:2)], table)

#--Select necessary columns
crime <- crime |> 
  dplyr::select(category, id, month, contains("location"))
    
#stop <- get_data(url = pth_stp, poly = coords_list, date = dt_list)
str(stop)
lapply(stop[,c(1:7)], table)

####### ANALYSE DATA #########
#--St-level crime 
crime |> 
    group_by(category) |>
    tally() |>
    arrange(desc(n))
    #ASB, violent crime and vehicle crime were top3 crimes

#--Stop and search
stop_cln <- stop |> 
    filter(outcome != "A no further action disposal") |>
    mutate(date = date(datetime)) |>
    mutate(week = isoweek(date)) |> 
    mutate(wday =wday(date)) |> 
    mutate(wday = case_when(
        wday == 1 ~ "Sunday",
        wday == 2 ~ "Monday",
        wday == 3 ~ "Tuesday",
        wday == 4 ~ "Wednesday",
        wday == 5 ~ "Thursday",
        wday == 6 ~ "Friday",
        wday == 7 ~ "Saturday"
    )) |>
    mutate(
        longitude = as.numeric(location.longitude),
        latitude = as.numeric(location.latitude)
        ) |>
    mutate(
        self_defined_ethnicity_upper = case_when(
            grepl("White", self_defined_ethnicity) ~ "White",
            grepl("Black", self_defined_ethnicity) ~ "Black",
            grepl("Asian", self_defined_ethnicity) ~ "Asian",
            grepl("Mixed", self_defined_ethnicity) ~ "Mixed",
            grepl("Other", self_defined_ethnicity) ~ "Other"
        )
    )

#---QC
table(stop_cln$self_defined_ethnicity_upper == stop_cln$officer_defined_ethnicity)
table(stop_cln$legislation)
unique(stop_cln$self_defined_ethnicity_upper)
table(is.na(stop_cln$self_defined_ethnicity_upper))
stop_cln[stop_cln$self_defined_ethnicity_upper != stop_cln$officer_defined_ethnicity,]
