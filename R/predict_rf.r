predict_rf <- function(list) {
  cat("Début de la fonction predict_rf.\n")
  
  pres_train <- list$pres_train
  backg_train <- list$backg_train
  pres_test <- list$pres_test
  backg_test <- list$backg_test
  shapefile_path <- list$shapefile_path
  output_folder <- list$output_folder
  input_file <- list$input_file
  
  cat("Chargement des fichiers raster et shapefile.\n")
  predictors_masked <- raster::brick(input_file)
  shapefile <- raster::shapefile(shapefile_path)
  shapefile_sf <- sf::st_as_sf(shapefile)

  # Créer le nom du sous-dossier en fonction du nom de la fonction utilisée
  subfolder_name <- paste0("RandomForest")
  
  # Créer le chemin complet du dossier de sortie
  output_folder1 <- file.path(output_folder, subfolder_name)
  
  # Vérifier si le dossier existe déjà, sinon le créer
  if (!file.exists(output_folder1)) {
    dir.create(output_folder1, recursive = TRUE)
  }
  
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
  
  cat("Conversion des coordonnées en data frames.\n")
  converted_points <- list()
  for (point in points_to_convert) {
    cat(paste("Conversion des coordonnées pour", point, ".\n"))
    converted_points[[point]] <- convert_coordinates_to_dataframe(sp::coordinates(list[[point]]))
  }
  
  cat("Création du vecteur de réponse pour les données d'entraînement.\n")
  response <- c(rep(1, nrow(converted_points[["pres_train"]])), rep(0, nrow(converted_points[["backg_train"]])))
  
  cat("Extraction des valeurs des prédicteurs pour les données d'entraînement.\n")
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
  
  cat("Entraînement du modèle Random Forest.\n")
  rng_dws <- ranger::ranger(formula = response ~ .,
                            data = evtrain2, 
                            num.trees = 1000,
                            probability = TRUE,
                            sample.fraction = bgNum / prNum,
                            case.weights = casewts,
                            num.threads = 6)
  
  cat("Prédiction des couches raster avec le modèle entraîné.\n")
  pred_rng_dws <- dismo::predict(predictors_masked, rng_dws,
                        fun = function(model, ...) dismo::predict(model, ...)$predictions[,"1"])
                        
  
  output_raster <- file.path(output_folder1, "rf_prediction_global.tif")
  terra::writeRaster(pred_rng_dws, output_raster, overwrite = TRUE)

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
  
  cat("Sauvegarde de la carte de prédiction.\n")
  output_plot <- file.path(output_folder1, "rf_prediction_global.png")
  ggplot2::ggsave(output_plot, plot = rfmap, width = 10, height = 8)
  
  cat("Prédiction sur l'ensemble de test.\n")
  envtest_presence <- raster::extract(predictors_masked, converted_points[["pres_test"]])
  envtest_background <- raster::extract(predictors_masked, converted_points[["backg_test"]])
  # Combine presence and background data for testing
  envtest <- rbind(envtest_presence, envtest_background)
  # Create response vector for testing data
  response1 <- c(rep(1, nrow(envtest_presence)), rep(0, nrow(envtest_background)))
  # Create data frame with response and predictors for testing data
  envtest <- data.frame(response1 = response1, envtest)
  # Remove rows with missing values
  envtest <- envtest[complete.cases(envtest), ]

    # Evaluate the model
  test_predictions <- dismo::predict(rng_dws, data = envtest[,-1])$predictions[, "1"]
  #e_rf <- dismo::evaluate(p = test_predictions[envtest$response1 == 1], a = test_predictions[envtest$response1 == 0])
  # Extract the equal sensitivity and specificity threshold
  #th1 <- threshold(e_rf, "equal_sens_spec")

  eval_rf <- predicts::pa_evaluate(p = test_predictions[envtest$response1 == 1], a = test_predictions[envtest$response1 == 0])

  th_rf <- eval_rf@thresholds$equal_sens_spec

  # Convert prediction to SpatRaster
  pred_rng_dws <- terra::rast(pred_rng_dws)

  # Apply threshold to prediction
  r_rf <- pred_rng_dws > th_rf
  rr_rf <- terra::ifel(r_rf == 0, NA, r_rf)
  # Calculate the area in km^2
  surface_pred_rf <- terra::expanse(rr_rf, unit = "km")
  
  # Save the predicted area raster
  output_raster <- file.path(output_folder1, "rf_predicted_area_with_th.tif")
  terra::writeRaster(rr_rf, output_raster, overwrite = TRUE)

  df_rr <- as.data.frame(rr_rf, xy = TRUE)
  # Tracer la carte avec ggplot2
  rf_th <- ggplot2::ggplot(data = df_rr, ggplot2::aes(x = x, y = y)) + 
    ggplot2::geom_raster(ggplot2::aes(fill = "Présence de l'espece")) +  # Utiliser une seule couleur (vert) avec une légende spécifique
    ggplot2::geom_polygon(data = ggplot2::fortify(shapefile), ggplot2::aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
    ggplot2::coord_equal() + 
    ggplot2::ggtitle("Random Forest model prediction de presence") + 
    ggplot2::theme_minimal() + 
    ggplot2::theme(legend.position = "right") +
    ggplot2::scale_fill_manual(name = "Légende", values = "green", labels = "Présence de l'espece")

    output_plot <- file.path(output_folder1, "rf_predicted_area_with_th.png")
    ggplot2::ggsave(output_plot, plot = rf_th, width = 10, height = 8)
  

  cat("Enregistrement des résultats .\n")
  results_rf <- data.frame(Model = "Random Forest", AUC = eval_rf@stats[["auc"]], Predicted_Area = surface_pred_rf)
  
  cat("Fin de la fonction predict_rf.\n")
  return(list(model = rng_dws, evaluation = results_rf, prediction = pred_rng_dws))
}

