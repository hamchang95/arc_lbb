######### SETTING ##########
#--Install / load packages
pacman::p_load(sp, data.table, rio, sf, here, leaflet, gstat, tidyverse, Metrics, scales, corrr, ggcorrplot, FactoMineR, factoextra, corrplot)

#--Import street-level asb data
asb <- import(here("3_output", "asb_with_nearest_distances.csv"))

#--Calculate count of crimes per location coordinate
asb_count <- asb |> 
    group_by(location.latitude, location.longitude) |>
    count() |> 
    ungroup() |> 
    inner_join(asb, by = c('location.latitude', 'location.longitude')) |> 
    distinct(location.latitude, location.longitude, .keep_all = TRUE) |>
    group_by(location.latitude, location.longitude) |>
    mutate(location_id = cur_group_id()) |>
    ungroup()

#--Store sf version of predictor and target
asb_x_sf <- asb_count |> 
    select(contains('location.'), starts_with('d_')) |>
    st_as_sf(coords = c("location.longitude", "location.latitude"))

asb_y_sf <- asb_count |>
    select(contains('location.'), n) |>
    st_as_sf(coords = c("location.longitude", "location.latitude"))

#--Get only numerical version
asb_x <- asb_count |> select(starts_with('d'))
asb_y <- asb_count |> select(n)

####### SPLIT ######
#--Create random indices
total_rows <- nrow(asb_count)
sample_size <- round(total_rows * 0.75)

set.seed(1234)
random_indices <- sample(1:total_rows, sample_size, replace = FALSE)

#--Create test sets using the random indices
x_train <- asb_x[dimnames(asb_x)[[1]] %in% random_indices,]
y_train <- asb_y[dimnames(asb_y)[[1]] %in% random_indices,]

#--Create training sets by excluding the indices used for the test set
x_test <- asb_x[!dimnames(asb_x)[[1]] %in% random_indices,]
y_test <- asb_y[!dimnames(asb_y)[[1]] %in% random_indices,]

#--Do the same for sf objects
x_train_sf <- asb_x_sf[dimnames(asb_x_sf)[[1]] %in% random_indices,]
y_train_sf <- asb_y_sf[dimnames(asb_y_sf)[[1]] %in% random_indices,]
x_test_sf <- asb_x_sf[!dimnames(asb_x_sf)[[1]] %in% random_indices,]
y_test_sf <- asb_y_sf[!dimnames(asb_y_sf)[[1]] %in% random_indices,]

#--Add x and y columns to sf objects
x_train_sf <- x_train_sf |> 
    mutate(x = st_coordinates(x_train_sf)[, 1], y = st_coordinates(x_train_sf)[, 2])
y_train_sf <- y_train_sf |> 
    mutate(x = st_coordinates(y_train_sf)[[, 1]], y = st_coordinates(y_train_sf)[, 2])
x_test_sf <- x_test_sf |> 
    mutate(x = st_coordinates(x_test_sf)[[, 1]], y = st_coordinates(x_test_sf)[[, 2]])
y_test_sf <- y_test_sf |> 
    mutate(x = st_coordinates(y_test_sf)[[, 1]], y = st_coordinates(y_test_sf)[[, 2]])

##### NORMALISE #####
#--Scale train set
x_train_norm <- scale(x_train)
mean(x_train_norm); sd(x_train_norm)

#--Scale test set using normalised train set's center and scale
x_test_norm <- scale(x_test, 
                    center = attr(x_train_norm, "scaled:center"),
                    scale = attr(x_train_norm, "scaled:scale"))

###### PCA ######
#--Apply PCA
x_train_pca <- PCA(x_train_norm, graph = FALSE, scale.unit = FALSE)

#--Check cumulative percentage of variance
x_train_pca$eig
# Comp 1-7 explains around 70% of total variance
# Hence, 7 is the number of components to keep

#--Apply PCA again with the selected number of components
x_train_pca <- PCA(x_train_norm, graph = FALSE, scale.unit = FALSE, ncp = 7)

#--Predict 
x_train_pred <- FactoMineR::predict.PCA(x_train_pca, x_train_norm)
x_test_pred <- FactoMineR::predict.PCA(x_train_pca, x_test_norm)

#--Extract PCA-transformed data
x_train_pred_df <- as.data.frame(x_train_pred$coord)
x_test_pred_df <- as.data.frame(x_test_pred$coord)

#--Add coordinates to the PCA-transformed data
x_train_pred_df <- cbind(x_train_pred_df, x = x_train_sf$x, y = x_train_sf$y)
x_test_pred_df <- cbind(x_test_pred_df, x = x_test_sf$x, y = x_test_sf$y)

#--Convert to spatial data frames
coordinates(x_train_pred_df) <- ~ x + y
coordinates(x_test_pred_df) <- ~ x + y
proj4string(x_train_pred_df) <- CRS("+proj=longlat +datum=WGS84")
proj4string(x_test_pred_df) <- CRS("+proj=longlat +datum=WGS84")

###### KRIGING ######
#--Combine PCA-transformed predictors with the target variable for training data
train_data <- cbind(x_train_pred_df, n = y_train_sf$n)
names(train_data)[8] <- "n" 

#--Combine PCA-transformed predictors with the target variable for test data
test_data <- cbind(x_test_pred_df, n = y_test_sf$n)
names(test_data)[8] <- "n"

#--Create variogram using PCA-transformed predictors
formula <- as.formula(paste("log(n) ~", paste(names(x_train_pred_df)[1:7],
collapse = " + ")))

lzn.vgm <- variogram(
    formula, 
    data = train_data, 
    width = 0.1)

#--Fit variogram model
lzn.fit <- fit.variogram(lzn.vgm, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)

#--Plot variogram model fit
plot(lzn.vgm, lzn.fit, main = "Variogram Model Fit", cutoff = max(lzn.vgm$dist))

#--Kriging over the test set
lzn.kriged <- krige(formula, train_data, test_data, model = lzn.fit, maxdist = 10, nmax = 50)

####### CREATE GRID #######
train_data |> head()
spd_data |> head()
#--Import Barnet shapefile
bnt_shp <- sf::st_read(here("1_data", "9_geo", "bnt_lad.json"), crs = 4326)
bnt_shp <- st_transform(bnt_shp, 4326)

spd <- sf::as_Spatial(st_geometry(bnt_shp), IDs = as.character(1:nrow(bnt_shp)))
  
spd_data <- bnt_shp
spd_data$geometry = NULL
spd_data <- as.data.frame(spd_data)
spd <- sp::SpatialPolygonsDataFrame(spd, data = spd_data)
  
#--Get the bounding box of the polygon and expand it slightly
bbox <- bbox(spd)
cellsize = 0.001
x_range <- seq(from = bbox[1,1] - cellsize, to = bbox[1,2] + cellsize, by = cellsize)
y_range <- seq(from = bbox[2,1] - cellsize, to = bbox[2,2] + cellsize, by = cellsize)
  
#--Create a data.frame of grid points
grid_points <- expand.grid(x = x_range, y = y_range)

coordinates(grid_points) <- ~ x + y
proj4string(grid_points) <- CRS("+proj=longlat +datum=WGS84")

# Create a dataframe for PCA transformation of grid points
grid_data <- as.data.frame(grid_points) |>
    distinct(y, x) |>
    left_join(st_drop_geometry(x_train_sf))
    st_drop_geometry() 
    select(-geometry, -x, -y)

# Normalize grid data using the same scaling parameters as the training data
grid_data_scaled <- scale(grid_data, center = attr(x_train_norm, "scaled:center"), scale = attr(x_train_norm, "scaled:scale"))

# Predict PCA components for grid data
grid_pca_pred <- predict(x_train_pca, newdata = grid_data_scaled)

# Add PCA components to grid data
grid_pca_df <- as.data.frame(grid_pca_pred$coord)
grid_data <- cbind(grid_data, grid_pca_df)

grid_data |> tail()

tail(x_train_sf[,c('x', 'y', 'd_pub')])
x_train_sf$x[1:5]
grid_data$x[1:5]
map(grid_data, ~sum(is.na(.x)))

# Convert grid data to SpatialPointsDataFrame
coordinates(grid_data) <- ~ x + y
proj4string(grid_data) <- CRS("+proj=longlat +datum=WGS84")

######## KRIGING OVER GRID #######
# Kriging over the grid
bnt.kriged <- krige(formula, train_data, grid_data, model = lzn.fit, maxdist = 10, nmax = 50)

# Retrieve interpolated values for test data
test_sf <- st_as_sf(test_data, coords =









#--Kriging over the grid
bnt.kriged <- krige(formula, train_data, grid_points, model = lzn.fit, maxdist = 10, nmax = 50)

# Retrieve interpolated values for test data
test_sf <- st_as_sf(test_data, coords = c("x", "y"), crs = 4326)
test_sf$krige_pred <- exp(lzn.kriged@data$var1.pred)
test_sf$variance <- exp(lzn.kriged@data$var1.var)

# Plot the kriged results
ggplot() +
  geom_sf(data = test_sf, aes(colour = krige_pred)) +
  scale_colour_gradient(low = "blue", high = "red", name = "Predicted Value") +
  ggtitle("Hotspot Map for Kriged Data") +
  geom_sf(data = bnt_shp, alpha = 0.1, lwd = 1.2) +
  theme_minimal()

# Evaluate kriging error
kriging_error <- rmse(test_data$n, test_sf$krige_pred)
kriging_error
