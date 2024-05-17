######### SETTING ##########
#--Install / load packages
pacman::p_load(sp, data.table, sf, here, leaflet, gstat, tidyverse, Metrics)

#--Import street-level crime data
crime <- rio::import(here::here("3_output", "crime_2024-05-09.csv")) |>
    dplyr::mutate(category = stringr::str_replace_all(category, "-", " ")) 

#--Filter ASB - the most prevalent type of crime
asb <- subset(crime, category == "anti social behaviour")

#--Calculate the frequency of ASB by location_id
asb_count <- asb |> 
    group_by(location.latitude, location.longitude) |>
    count() |> 
    ungroup() |> 
    inner_join(asb, by =c('location.latitude', 'location.longitude')) |> 
    distinct(location.latitude, location.longitude, .keep_all = TRUE) |>
    select(location.latitude, location.longitude, n) |>
    group_by(location.latitude, location.longitude) |>
    mutate(location_id = group_indices())

names(asb_count)[1:2] <- c("y", "x")
seq_len(nrow(asb_count))
#--Define training set 
total_rows <- nrow(asb_count)
sample_size <- round(total_rows * 0.75)

# Generate random indices
set.seed(1234)
random_indices <- sample(1:total_rows, sample_size, replace = FALSE)

# Create the test set using the random indices
train_asb <- asb_count |> 
filter(!location_id %in% random_generator) 

t
# Create the training set by excluding the indices used for the test set
test_asb <-  asb_count |> filter(location_id %in% random_generator)

  #leaflet::leaflet(st_as_sf(train_asb, coords = c('location.longitude', 'location.latitude'), crs=4326)) |>
    #leaflet::addTiles() |>
    #leaflet::addCircleMarkers(radius = 1, fillOpacity = 0.001) 

############# BUILD VARIOGRAM ###########
coordinates(train_asb) <- c("x", "y")
proj4string(train_asb) <- CRS("+proj=longlat +datum=WGS84")
coordinates(test_asb) <- c("x", "y")
proj4string(test_asb) <- CRS("+proj=longlat +datum=WGS84")
lzn.vgm <- variogram(log(n) ~ x+y, train_asb, width=0.1)
lzn.fit = fit.variogram(lzn.vgm, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)

#--Plot the curve function
plot(lzn.vgm, lzn.fit, main = "Variogram Model Fit", cutoff = max(lzn.vgm$dist))
 # When the curve plateaus, the point pairs are no longer spatially correlated
# Define the bounding box

#--Import Barnet shapefile
bnt_shp <- sf::st_read(here("1_data", "9_geo", "bnt_lad.json"), crs = 4326) 
bnt_shp <- st_transform(bnt_shp, 4326)

spd <- sf::as_Spatial(st_geometry(bnt_shp), IDs = as.character(1:nrow(bnt_shp)))

spd_data = bnt_shp
spd_data$geometry = NULL
spd_data <- as.data.frame(spd_data)
spd <- sp::SpatialPolygonsDataFrame(spd, data = spd_data)

# Get the bounding box of the polygon and expand it slightly
bbox <- bbox(spd)
cellsize = 0.001
x_range <- seq(from = bbox[1,1] - cellsize, to = bbox[1,2] + cellsize, by = cellsize)
y_range <- seq(from = bbox[2,1] - cellsize, to = bbox[2,2] + cellsize, by = cellsize)

# Create a data.frame of grid points
grid_points <- expand.grid(x = x_range, y = y_range)

# Convert grid points to SpatialPoints
grid <- SpatialPoints(grid_points, proj4string = CRS("+proj=longlat +datum=WGS84"))

# Step 4: Filter grid points to include only those within the Barnet polygon
grid <- grid[spd, ]

# Convert grid back to sf object if needed
grid_sf <- st_as_sf(grid)

# Optionally, visualize the result to ensure it covers the entire polygon
plot(st_geometry(bnt_shp), col = 'lightblue', border = 'darkblue')
plot(grid_sf, add = TRUE, col = 'red', pch = 16)

coordinates(grid)|>head()

######## KRIGE #######
lzn.kriged <-krige(log(n) ~ x + y, train_asb, test_asb, model = lzn.fit, maxdist=10, nmax=50)

# Retrieve interpolated values
predicted <- lzn.kriged@data$var1.pred |>
  as.data.frame() |>
  rename(krige_pred = 1) 

variance <- lzn.kriged@data$var1.var |>
  as.data.frame() |>
  rename(variance = 1) 

grid_sf <- st_as_sf(test_asb, coords = c("x", "y"), crs = 4326)
grid_sf$krige_pred <- predicted$krige_pred
grid_sf$variance <- variance$variance
length(grid_sf$krige_pred)
ggplot() +
  geom_sf(data = grid_sf, aes(colour = krige_pred)) +
  scale_colour_gradient(low = "blue", high = "red", name = "Predicted Value") +
  ggtitle("Hotspot Map for Kriged Data") +
  geom_sf(data = bnt_shp, alpha=0.1, lwd = 1.2)+
  theme_minimal()


kriging_error = rmse(test_asb$n, exp(grid_sf$krige_pred))

kriging_error
range(asb_count$n)