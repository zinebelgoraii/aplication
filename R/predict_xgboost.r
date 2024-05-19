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
  envtrain_presence <- raster::extract(predictors_masked, pres_train)
  envtrain_background <- raster::extract(predictors_masked, backg_train)
  
  # Combine presence and background data
  envtrain <- rbind(envtrain_presence, envtrain_background)
  
  # Create response vector for training data
  response <- c(rep(1, nrow(envtrain_presence)), rep(0, nrow(envtrain_background)))
  
  # Create data frame with response and predictors for training data
  envtrain <- data.frame(response = response, envtrain)
  
  # Testing data extraction
  cat("Extraction des valeurs des prédicteurs pour les données de test.\n")
  envtest_presence <- raster::extract(predictors_masked, pres_test)
  envtest_background <- raster::extract(predictors_masked, backg_test)
  
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
  
  # Variable importance
  cat("Calcul de l'importance des variables.\n")
  importance_matrix <- xgboost::xgb.importance(
    feature_names = colnames(xgb_train), 
    model = xgb_model
  )
  
  # Plot variable importance
  xgboost::xgb.plot.importance(importance_matrix)
  
  # Predictions on test data
  cat("Prédiction sur les données de test.\n")
  xgb_preds <- dismo::predict(xgb_model, xgb_test, reshape = TRUE)
  xgb_preds <- as.data.frame(xgb_preds)
  
  xgb_preds$PredictedClass <- ifelse(xgb_preds$xgb_preds >= 0.5, "1", "0")
  xgb_preds$ActualClass <- levels(envtrain$response)[y_test + 1]
  
  # Accuracy
  accuracy <- sum(xgb_preds$PredictedClass == xgb_preds$ActualClass) / nrow(xgb_preds)
  cat("Précision du modèle: ", accuracy, "\n")
  
  # Confusion matrix
  cm <- caret::confusionMatrix(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass))
  cfm <- dplyr::as_tibble(cm$table)
  cvms::plot_confusion_matrix(cfm, target_col = "Reference", prediction_col = "Prediction", counts_col = "n")
  # Spatial predictions
  
  cat("Prédictions spatiales avec le modèle XGBoost.\n")
  ext_shapefile <- raster::extent(shapefile)
  predictors_matrix <-  raster::as.matrix(predictors_masked)
  predictions <- dismo::predict(xgb_model, newdata = predictors_matrix,ext = ext_shapefile)
  # Create raster for predictions
  predictions_raster <- raster::raster(ext_shapefile, nrows = nrow(predictors_masked), ncols = ncol(predictors_masked))
  raster::values(predictions_raster) <- predictions
  raster::projection(predictions_raster) <- raster::projection(predictors_masked)
  
  # Mask predictions with shapefile
  pre <- raster::mask(predictions_raster, shapefile)
  
  # Convert raster to dataframe
  miaw <- as.data.frame(pre, xy = TRUE)
  
  # Plot spatial predictions
  ggplot2::ggplot(miaw, ggplot2::aes(x = x, y = y, fill = layer)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradientn(colors = terrain.colors(10)[10:1]) +
    ggplot2::labs(fill = "Intensité de présence d'arganier") +
    ggplot2::coord_equal() +
    ggplot2::ggtitle("Prédictions spatiales avec XGBoost") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "right") +
    ggplot2::geom_polygon(data = shapefile, ggplot2::aes(x = long, y = lat, group = group), fill = NA)
  
  # Save the plot
  output_plot <- file.path(output_folder, "xgboost_predictions_plot.png")
  ggplot2::ggsave(output_plot, width = 10, height = 8)

  cat("Fin de la fonction predict_xgboost.\n")
  return(list(model = xgb_model, importance = importance_matrix, predictions = predictions_raster, accuracy = accuracy, confusion_matrix = cm))
}