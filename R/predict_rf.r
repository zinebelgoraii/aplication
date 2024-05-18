predict_rf <- function(list) {
  message("Début de la fonction predict_rf.")
  
  pres_train <- list$pres_train
  backg_train <- list$backg_train
  pres_test <- list$pres_test
  backg_test <- list$backg_test
  shapefile_path <- list$shapefile_path
  output_folder <- list$output_folder
  input_file <- list$input_file
  
  message("Chargement des fichiers raster et shapefile.")
  predictors_masked <- raster::brick(input_file)
  shapefile <- raster::shapefile(shapefile_path)
  shapefile_sf <- sf::st_as_sf(shapefile)
  
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
  
  points_to_convert <- c("pres_train", "pres_test", "backg_train", "backg_test")
  
  message("Conversion des coordonnées en data frames.")
  converted_points <- list()
  for (point in points_to_convert) {
    message(paste("Conversion des coordonnées pour", point, "."))
    converted_points[[point]] <- convert_coordinates_to_dataframe(sp::coordinates(list[[point]]))
  }
  
  message("Création du vecteur de réponse pour les données d'entraînement.")
  response <- c(rep(1, nrow(converted_points[["pres_train"]])), rep(0, nrow(converted_points[["backg_train"]])))
  
  message("Extraction des valeurs des prédicteurs pour les données d'entraînement.")
  envtrain_presence <- raster::extract(predictors_masked, converted_points[["pres_train"]])
  envtrain_background <- raster::extract(predictors_masked, converted_points[["backg_train"]])
  
  envtrain <- rbind(envtrain_presence, envtrain_background)
  envtrain <- data.frame(response = response, envtrain)
  envtrain <- envtrain[complete.cases(envtrain), ]
  
  evtrain2 <- envtrain
  evtrain2$response <- as.factor(evtrain2$response)
  
  prNum <- as.numeric(table(evtrain2$response)["1"])
  bgNum <- as.numeric(table(evtrain2$response)["0"])
  casewts <- ifelse(evtrain2$response == 1, 1, bgNum / prNum)
  
  message("Entraînement du modèle Random Forest.")
  rng_dws <- ranger::ranger(formula = response ~ .,
                            data = evtrain2, 
                            num.trees = 1000,
                            probability = TRUE,
                            sample.fraction = bgNum / prNum,
                            case.weights = casewts,
                            num.threads = 6)
  
  message("Prédiction des couches raster avec le modèle entraîné.")
  pred_rng_dws <- dismo::predict(predictors_masked, rng_dws,
                                 fun = function(model, ...) predict(model, ...)$predictions[,"1"])
  
  spdf <- as(pred_rng_dws, "SpatialPixelsDataFrame")
  df_dws_cl1 <- as.data.frame(spdf, xy = TRUE)
  
  rfmap <- ggplot2::ggplot(df_dws_cl1, ggplot2::aes(x = x, y = y, fill = layer)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradientn(colors = terrain.colors(10)[10:1]) +
    ggplot2::labs(fill = "likelihood") +
    ggplot2::coord_equal() +
    ggplot2::ggtitle("RF model prediction") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "right") +
    ggplot2::geom_polygon(data = shapefile, ggplot2::aes(x = long, y = lat, group = group), fill = NA)
  
  message("Sauvegarde de la carte de prédiction.")
  output_plot <- file.path(output_folder, "randomForest_plot.png")
  ggplot2::ggsave(output_plot, plot = rfmap, width = 10, height = 8)
  
  message("Prédiction sur l'ensemble de test.")
  response_test <- c(rep(1, nrow(converted_points[["pres_test"]])), rep(0, nrow(converted_points[["backg_test"]])))
  envtest_presence <- raster::extract(predictors_masked, converted_points[["pres_test"]])
  envtest_background <- raster::extract(predictors_masked, converted_points[["backg_test"]])
  envtest <- rbind(envtest_presence, envtest_background)
  envtest <- data.frame(response = response_test, envtest)
  envtest <- envtest[complete.cases(envtest), ]
  
  p <- envtest[envtest$response == 1, -1]
  a <- envtest[envtest$response == 0, -1]
  
  test_data <- rbind(p, a)
  test_labels <- c(rep(1, nrow(p)), rep(0, nrow(a)))
  
  predictions <- predict(rng_dws, data = test_data)$predictions[, "1"]
  
  message("Calcul de l'AUC.")
  roc_obj <- pROC::roc(test_labels, predictions)
  auc_value <- pROC::auc(roc_obj)
  
  message("Calcul de la superficie prédite.")
  predicted_area <- sum(pred_rng_dws[] >= 0.5, na.rm = TRUE) * raster::res(pred_rng_dws)[1] * raster::res(pred_rng_dws)[2]
  
  message("Enregistrement des résultats dans un fichier Excel.")
  results <- data.frame(Model = "Random Forest", AUC = auc_value, Predicted_Area = predicted_area)
  
  message("Fin de la fonction predict_rf.")
  return(list(model = rng_dws, evaluation = results, prediction = pred_rng_dws))
}
