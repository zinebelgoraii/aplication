predict_rf <- function(list) {
  pres_train <- list$pres_train
  backg_train <- list$backg_train
  pres_test <- list$pres_test
  backg_test <- list$backg_test
  shapefile_path <- list$shapefile_path
  output_folder <- list$output_folder
  input_file <- list$input_file
  
  predictors_masked <- raster::brick(input_file)
  shapefile <- raster::shapefile(shapefile_path)
  shapefile_sf <- sf::st_as_sf(shapefile)
  
  # Nécessaire pour la conversion de coordonnées
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
  
  # Créer une liste des noms des points à convertir
  points_to_convert <- c("pres_train", "pres_test", "backg_train", "backg_test")
  
  # Boucle sur la liste et convertit les coordonnées en data frames
  converted_points <- list()
  for (point in points_to_convert) {
    converted_points[[point]] <- convert_coordinates_to_dataframe(sp::coordinates(list[[point]]))
  }
  
  # Créer le vecteur de réponse pour les données d'entraînement
  response <- c(rep(1, nrow(converted_points[["pres_train"]])), rep(0, nrow(converted_points[["backg_train"]])))
  
  # Extraire les valeurs des prédicteurs pour les données d'entraînement
  envtrain_presence <- raster::extract(predictors_masked, converted_points[["pres_train"]])
  envtrain_background <- raster::extract(predictors_masked, converted_points[["backg_train"]])
  
  # Combiner les données de présence et d'arrière-plan
  envtrain <- rbind(envtrain_presence, envtrain_background)
  
  # Créer un data frame avec la réponse et les valeurs des prédicteurs
  envtrain <- data.frame(response = response, envtrain)
  
  # Supprimer les lignes avec des valeurs manquantes
  envtrain <- envtrain[complete.cases(envtrain), ]
  
  evtrain2 <- envtrain
  evtrain2$response <- as.factor(evtrain2$response)
  
  prNum <- as.numeric(table(evtrain2$response)["1"])
  bgNum <- as.numeric(table(evtrain2$response)["0"])
  casewts <- ifelse(evtrain2$response == 1, 1, bgNum / prNum)
  
  rng_dws <- ranger::ranger(formula = response ~ .,
                            data = evtrain2, 
                            num.trees = 1000,
                            probability = TRUE,
                            sample.fraction = bgNum / prNum,
                            case.weights = casewts,
                            num.threads = 6)
  # Prédire les couches raster
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
  
  output_plot <- file.path(output_folder, "randomForest_plot.png")
  ggplot2::ggsave(output_plot, plot = rfmap, width = 10, height = 8)
  
  # Prédire sur l'ensemble de test
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
  
  # Calculer l'AUC en utilisant pROC
  roc_obj <- pROC::roc(test_labels, predictions)
  auc_value <- pROC::auc(roc_obj)
  
  # Calculer la superficie prédite
  predicted_area <- sum(pred_rng_dws[] >= 0.5, na.rm = TRUE) * raster::res(pred_rng_dws)[1] * raster::res(pred_rng_dws)[2]
  
  # Enregistrer les résultats dans Excel
  results <- data.frame(Model = "Random Forest", AUC = auc_value, Predicted_Area = predicted_area)
  return(list(model = rng_dws, evaluation = results, prediction = pred_rng_dws))
}
