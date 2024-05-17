#' Predict with Maxent
#' 
#' This function performs predictions with Maxent.
#' 
#' @param list A list containing train and test data.
#' @param output_folder Path to the output folder.
#' @return NULL
#' @export
predict <- function(list) {
  pres_train <- list$pres_train
  backg_train <- list$backg_train
  pres_test <- list$pres_test
  backg_test <- list$backg_test
  shapefile_path <-  list$shapefile_path
  output_folder <- list$output_folder
  predictors_masked <- brick(input_file)
  
  shapefile <- shapefile(shapefile_path)
  shapefile_sf <- st_as_sf(shapefile)
  
  # Définir la fonction de conversion
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
  points_to_convert <- c("pres_train","pres_test", "backg_train", "backg_test")
  
  # Boucle sur la liste et convertit les coordonnées en data frames
  converted_points <- list()
  for (point in points_to_convert) {
    converted_points[[point]] <- convert_coordinates_to_dataframe(coordinates(train_test_data[[point]]))
  }
 
  output_plot <- file.path(output_folder, "important_predictors_plot.png")
  maxent()
  mx <- maxent(predictors_masked, converted_points[["pres_train"]])
  plot(mx)
  
  ggsave(output_plot, plot = mx, width = 10, height = 8)
  
  e_mx <- evaluate(converted_points[["pres_test"]],converted_points[["backg_test"]], mx, predictors_masked)
  print(e_mx)
  
  # Capturer la sortie de print(e_mx)
  output_text <- capture.output(print(e_mx))
  
  # Définir le chemin de fichier pour enregistrer le résultat
  output_file <- file.path(output_folder, "evaluation_result.txt")
  
  # Écrire le résultat dans un fichier texte
  writeLines(output_text, output_file)
  
  # Afficher un message pour indiquer que le résultat a été enregistré avec succès
  cat("Evaluation result saved successfully at:", output_file, "\n")
  
  p_mx <- predict(predictors_masked, mx)
  
  df_maxent <- as.data.frame(p_mx, xy = TRUE)
  
  maxentmap <- ggplot(df_maxent, aes(x = x, y = y, fill = layer)) + 
    geom_raster() + 
    scale_fill_gradientn(colors = terrain.colors(10)[10:1]) + 
    labs(fill = "Likelihood") + 
    coord_equal() + 
    ggtitle("Occurrence prediction with Maxent model") + 
    theme_minimal() + 
    theme(legend.position = "right") + 
    geom_polygon(data = shapefile, aes(x = long, y = lat, group = group),  fill = NA)
  
  output_plot <- file.path(output_folder, "maxent_plot.png")
  ggsave(output_plot, plot = maxentmap, width = 10, height = 8)
}

#predict_maxent(train_test_data)




