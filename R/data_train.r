data_train <- function(list, input_file) {
  data_coords <- list$data_coords
  shapefile_path <- list$shapefile_path
  output_folder <- list$output_folder
  predictors_masked <- raster::brick(input_file)
  
  shapefile <- raster::shapefile(shapefile_path)
  shapefile_sf <- sf::st_as_sf(shapefile)
  
  set.seed(0)
  group <- dismo::kfold(data_coords, 5)
  pres_train <- data_coords[group != 1, ]
  pres_test <- data_coords[group == 1, ]
  
  set.seed(10)
  backg <- dismo::randomPoints(predictors_masked, n = 10000, ext = raster::extent(shapefile), extf = 1.25)
  colnames(backg) <- c('x', 'y')
  group <- dismo::kfold(backg, 5)
  backg_train <- backg[group != 1, ]
  backg_test <- backg[group == 1, ]
  
  if (!is.null(pres_train) && !is.null(pres_test) && !is.null(backg_train) && !is.null(backg_test)) {
    message("Successfully generated training and testing data.")
  } else {
    warning("Failed to generate training and testing data.")
  }
  
  # Returning the relevant data as a list
  return(list(
    pres_train = pres_train,
    pres_test = pres_test,
    backg_train = backg_train,
    backg_test = backg_test,
    shapefile_path = shapefile_path,
    output_folder = output_folder,
    input_file = input_file
  ))
}


#train_test_data <- data_train(data_list)

