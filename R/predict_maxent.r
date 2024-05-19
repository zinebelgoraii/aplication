predict_maxent <- function(list) {
  cat("Début de la fonction predict_maxent.\n")
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
  subfolder_name <- paste0("maxent")
  
  # Créer le chemin complet du dossier de sortie
  output_folder1 <- file.path(output_folder, subfolder_name)
  
  # Vérifier si le dossier existe déjà, sinon le créer
  if (!file.exists(output_folder1)) {
    dir.create(output_folder1, recursive = TRUE)
  }
  
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
  
  cat("Conversion des coordonnées en data frames.\n")
  # Créer une liste des noms des points à convertir
  points_to_convert <- c("pres_train", "pres_test", "backg_train", "backg_test")
  
  # Boucle sur la liste et convertit les coordonnées en data frames
  converted_points <- list()
  for (point in points_to_convert) {
    converted_points[[point]] <- convert_coordinates_to_dataframe(sp::coordinates(list[[point]]))
  }
  
  cat("Entraînement du modèle Maxent.\n")

  mx <- dismo::maxent(predictors_masked, converted_points[["pres_train"]])
  #e_mx <- dismo::evaluate(converted_points[["pres_test"]], converted_points[["backg_test"]], mx, predictors_masked)
  # Convert the evaluation results to a data frame
  #evaluation_df <- data.frame(Model = "Maxent", AUC = e_mx@auc)
  
  cat("Prediction du modèle Maxent.\n")
  p_mx <- dismo::predict(predictors_masked, mx)

  output_raster <- file.path(output_folder1, "maxent_prediction_global.tif")
  terra::writeRaster(p_mx, output_raster, overwrite = TRUE)
  
  spdf <- as(p_mx, "SpatialPixelsDataFrame")
  df_maxent <- as.data.frame(spdf, xy = TRUE)
  
  maxentmap <- ggplot2::ggplot(df_maxent,  ggplot2::aes(x = x, y = y, fill = layer)) + 
    ggplot2::geom_raster() + 
    ggplot2::scale_fill_gradientn(colors = terrain.colors(10)[10:1]) + 
    ggplot2::labs(fill = "Likelihood") + 
    ggplot2::coord_equal() + 
    ggplot2::ggtitle("Maxent model prediction") + 
    ggplot2::theme_minimal() + 
    ggplot2::theme(legend.position = "right") + 
    ggplot2::geom_polygon(data = shapefile, ggplot2::aes(x = long, y = lat, group = group), fill = NA)
  
  output_plot <- file.path(output_folder1, "maxent_prediction_global.png")
  ggplot2::ggsave(output_plot, plot = maxentmap, width = 10, height = 8)
  
  # Assuming p_mx is a  SpatRaster
  p_mx <- terra::rast(p_mx) # to Convert RasterLayer to SpatRaster use raster()

  eval_maxent <- predicts::pa_evaluate(pres_test, backg_test, mx, predictors_masked)

  th_maxent <- eval_maxent@thresholds$equal_sens_spec #####loop for all

  r_maxent <- p_mx > th_maxent 
  rr_maxent <- terra::ifel(r_maxent == 0, NA, r_maxent)
  surface_pred_maxent <- terra::expanse(rr_maxent, unit="km")
  

  # Save the predicted area raster
  output_raster <- file.path(output_folder1, "maxent_predicted_area_with_th.tif")
  terra::writeRaster(rr_maxent, output_raster, overwrite = TRUE)

  df_rr <- as.data.frame(rr_maxent, xy = TRUE)
  # Tracer la carte avec ggplot2
  maxent_th <- ggplot2::ggplot(data = df_rr, ggplot2::aes(x = x, y = y)) + 
    ggplot2::geom_raster(ggplot2::aes(fill = "Présence de l'espece")) +  # Utiliser une seule couleur (vert) avec une légende spécifique
    ggplot2::geom_polygon(data = ggplot2::fortify(shapefile), ggplot2::aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
    ggplot2::coord_equal() + 
    ggplot2::ggtitle("Maxent model prediction de presence") + 
    ggplot2::theme_minimal() + 
    ggplot2::theme(legend.position = "right") +
    ggplot2::scale_fill_manual(name = "Légende", values = "green", labels = "Présence de l'espece")

    output_plot <- file.path(output_folder1, "maxent_predicted_area_with_th.png")
    ggplot2::ggsave(output_plot, plot = maxent_th, width = 10, height = 8)
  
  # Save results
  results_maxent <- data.frame(Model = "Maxent", AUC = eval_maxent@stats[["auc"]], Predicted_Area = surface_pred_maxent)
  return(list(model = mx, evaluation = results_maxent, prediction = p_mx))

}