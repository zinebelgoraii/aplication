#' Predict with XGBoost Model
#'
#' This function trains an XGBoost model and predicts spatial presence using environmental data.
#'
#' @param list A list containing the following elements:
#'   \describe{
#'     \item{pres_train}{SpatialPoints for training presence data}
#'     \item{backg_train}{SpatialPoints for training background data}
#'     \item{pres_test}{SpatialPoints for testing presence data}
#'     \item{backg_test}{SpatialPoints for testing background data}
#'     \item{shapefile_path}{Path to the shapefile}
#'     \item{output_folder}{Path to the output folder}
#'     \item{input_file}{Path to the input raster file}
#'   }
#' @return A list containing the XGBoost model, variable importance, predictions, and evaluation metrics.
#' @export
predict_xgboost <- function(list) {
  cat("Début de la fonction predict_xgboost.\n")
  
  # Extract elements from the list
  pres_train <- list$pres_train
  backg_train <- list$backg_train
  pres_test <- list$pres_test
  backg_test <- list$backg_test
  shapefile_path <- list$shapefile_path
  output_folder <- list$output_folder
  input_file <- list$input_file
  
  # Load raster and shapefile
  cat("Chargement des fichiers raster et shapefile.\n")
  predictors_masked <- raster::brick(input_file)
  shapefile <- raster::shapefile(shapefile_path)
  shapefile_sf <- sf::st_as_sf(shapefile)

  # Créer le nom du sous-dossier en fonction du nom de la fonction utilisée
  subfolder_name <- paste0("XGBoost")
  
  # Créer le chemin complet du dossier de sortie
  output_folder1 <- file.path(output_folder, subfolder_name)
  
  # Vérifier si le dossier existe déjà, sinon le créer
  if (!file.exists(output_folder1)) {
    dir.create(output_folder1, recursive = TRUE)
  }
  
  # Convert coordinates to dataframe
  convert_coordinates_to_dataframe <- function(coordinates) {
    if (inherits(coordinates, "matrix")) {
      data_frame <- as.data.frame(coordinates)
      colnames(data_frame) <- c("X", "Y")
      return(data_frame)
    } else {
      warning("Input is not a matrix.")
      return(NULL)
    }
  }
  
  # Create a list of points to convert
  points_to_convert <- c("pres_train", "pres_test", "backg_train", "backg_test")
  
  # Loop through the list and convert coordinates to data frames
  cat("Conversion des coordonnées en data frames.\n")
  converted_points <- list()
  for (point in points_to_convert) {
    cat(paste("Conversion des coordonnées pour", point, ".\n"))
    converted_points[[point]] <- convert_coordinates_to_dataframe(sp::coordinates(list[[point]]))
  }
  
  # Training data extraction
  cat("Extraction des valeurs des prédicteurs pour les données d'entraînement.\n")
  envtrain_presence <- raster::extract(predictors_masked, converted_points[["pres_train"]])
  envtrain_background <- raster::extract(predictors_masked,converted_points[["backg_train"]] )
  
  # Combine presence and background data
  envtrain <- rbind(envtrain_presence, envtrain_background)
  
  # Create response vector for training data
  response <- c(rep(1, nrow(envtrain_presence)), rep(0, nrow(envtrain_background)))
  
  # Create data frame with response and predictors for training data
  envtrain <- data.frame(response = response, envtrain)
  
  # Testing data extraction
  cat("Extraction des valeurs des prédicteurs pour les données de test.\n")
  envtest_presence <- raster::extract(predictors_masked, converted_points[["pres_test"]])
  envtest_background <- raster::extract(predictors_masked, converted_points[["backg_test"]])
  
  # Combine presence and background data for testing
  envtest <- rbind(envtest_presence, envtest_background)
  
  # Create response vector for testing data
  response1 <- c(rep(1, nrow(envtest_presence)), rep(0, nrow(envtest_background)))
  
  # Create data frame with response and predictors for testing data
  envtest <- data.frame(response1 = response1, envtest)
  
  # Train XGBoost model
  cat("Entraînement du modèle XGBoost.\n")
  envtrain$response <- as.factor(envtrain$response)
  envtest$response1 <- as.factor(envtest$response1)
  
  y_train <- as.integer(envtrain$response) - 1
  y_test <- as.integer(envtest$response1) - 1
  X_train <- dplyr::select(envtrain, -response)
  X_test <- dplyr::select(envtest, -response1)
  
  xgb_train <- xgboost::xgb.DMatrix(data = as.matrix(X_train), label = y_train)
  xgb_test <- xgboost::xgb.DMatrix(data = as.matrix(X_test), label = y_test)
  
  xgb_params <- list(
    booster = "gbtree",
    eta = 0.01,
    max_depth = 8,
    gamma = 4,
    subsample = 0.75,
    colsample_bytree = 1,
    objective = "binary:logistic",
    eval_metric = "mlogloss"
  )
  
  xgb_model <- xgboost::xgb.train(
    params = xgb_params,
    data = xgb_train,
    nrounds = 100,
    verbose = 1
  )

  ext_shapefile <- raster::extent(shapefile)
  cat("prediction du modèle XGBoost.\n")
  # Ou si vous préférez une matrice :
  predictors_matrix <- raster::as.matrix(predictors_masked)

  # Faire des prédictions avec votre modèle XGBoost
  predictions <- raster::predict(xgb_model, newdata = predictors_matrix, ext = ext_shapefile)

  cat("evaluation du modèle XGBoost.\n")

  # Evaluate the model
  test <- dismo::predict(xgb_model, newdata = as.matrix(X_test))

  e_xgboost1 <- predicts::pa_evaluate(p = test[y_test == 1], a = test[y_test == 0])

  thxgboost <- e_xgboost1@thresholds$equal_sens_spec

  # Create an empty raster with the same dimensions as the input raster
  predictions_raster <- raster::raster(ext_shapefile, nrows = nrow(predictors_masked), ncols = ncol(predictors_masked))

  # Assign the predictions to the raster cells
  raster::values(predictions_raster) <- predictions

  # Optionally, set the projection of the raster if necessary
  raster::projection(predictions_raster) <- raster::projection(predictors_masked)


  pre <- raster::mask(predictions_raster, shapefile)

  # Save the predicted area raster
  output_raster <- file.path(output_folder1, "xgb_prediction_global.tif")
  terra::writeRaster(pre, output_raster, overwrite = TRUE)

  spdf <- as(pre, "SpatialPixelsDataFrame")    
  #Convertir le raster en data frame
  df_xg <- as.data.frame(spdf, xy = TRUE)

  xgb_map <- ggplot2::ggplot(df_xg, ggplot2::aes(x = x, y = y, fill = layer)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradientn(colors = terrain.colors(10)[10:1]) +
    ggplot2::labs(fill = "likelihood") +
    ggplot2::coord_equal() +
    ggplot2::ggtitle("XGBoost model prediction") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "right") +
    ggplot2::geom_polygon(data = shapefile, ggplot2::aes(x = long, y = lat, group = group), fill = NA)
  
  cat("Sauvegarde de la carte de prédiction.\n")
  output_plot <- file.path(output_folder1, "xgb_prediction_global.png")
  ggplot2::ggsave(output_plot, plot = xgb_map, width = 10, height = 8)
  
  cat("prediction de la carte avec th.\n")
  pre1 <- terra::rast(pre)

  # Apply threshold to prediction
  rxgboost <- pre1 > thxgboost

  rr1xgboost <- terra::ifel(rxgboost == 0, NA, rxgboost)

  # Save the predicted area raster
  output_raster <- file.path(output_folder1, "xgb_predicted_area_with_th.tif")
  terra::writeRaster(rr1xgboost, output_raster, overwrite = TRUE)

  # Calculate the area in km^2
  surface_pred_xgb <- terra::expanse(rr1xgboost, unit = "km")

  df_xgb <- as.data.frame(rr1xgboost, xy = TRUE)
  # Tracer la carte avec ggplot2
  xgb_th <- ggplot2::ggplot(data = df_xgb, ggplot2::aes(x = x, y = y)) + 
    ggplot2::geom_raster(ggplot2::aes(fill = "Présence de l'espece")) +  # Utiliser une seule couleur (vert) avec une légende spécifique
    ggplot2::geom_polygon(data = ggplot2::fortify(shapefile), ggplot2::aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
    ggplot2::coord_equal() + 
    ggplot2::ggtitle("XGBoost model prediction de presence avec th") + 
    ggplot2::theme_minimal() + 
    ggplot2::theme(legend.position = "right") +
    ggplot2::scale_fill_manual(name = "Légende", values = "green", labels = "Présence de l'espece")

  # Save the plot
  output_plot <- file.path(output_folder1, "xgboost_predicted_area_with_th.png")
  ggplot2::ggsave(output_plot, width = 10, height = 8)
 
  # Save results
  results_xgboost <- data.frame(Model = "XGBoost", AUC = e_xgboost1@stats[["auc"]], Predicted_Area = surface_pred_xgb$area)

  return(list(model = xgb_model, evaluation = results_xgboost, prediction = pre))

}
