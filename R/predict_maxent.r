predict_maxent <- function(list) {
  
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
  dismo::maxent()
  mx <- dismo::maxent(predictors_masked, converted_points[["pres_train"]])
  e_mx <- dismo::evaluate(converted_points[["pres_test"]], converted_points[["backg_test"]], mx, predictors_masked)
  
  # Convert the evaluation results to a data frame
  evaluation_df <- data.frame(Model = "Maxent", AUC = e_mx@auc)
  
  p_mx <- dismo::predict(predictors_masked, mx)
  
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
  
  output_plot <- file.path(output_folder, "maxent_plot.png")
  ggplot2::ggsave(output_plot, plot = maxentmap, width = 10, height = 8)
  
  # Calculate predicted area
  predicted_area <- sum(p_mx[] >= 0.5, na.rm = TRUE) * raster::res(p_mx)[1] * raster::res(p_mx)[2]
  
  # Save results to Excel
  results <- data.frame(Model = "Maxent", AUC = e_mx@auc, Predicted_Area = predicted_area)
  return(list(model = mx, evaluation = results, prediction = p_mx))
}