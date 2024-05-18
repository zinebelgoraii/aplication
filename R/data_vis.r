#' Generate map
#' 
#' This function generates a map from a dataset and a shapefile.
#' 
#' @param dataset_path Path to the dataset (CSV file).
#' @param shapefile_path Path to the shapefile.
#' @param output_folder Path to the output folder.
#' @return A list containing coordinates and shapefile path.
#' @export
# Load necessary libraries

data_vis <- function(dataset_path, shapefile_path, output_folder) {
  # Vérifier si le fichier existe
  if (!file.exists(dataset_path)) {
    stop("File does not exist.")
  }
  # Vérifier si le fichier est un fichier CSV
  if (!grepl("\\.csv$", dataset_path, ignore.case = TRUE)) {
    stop("File is not a CSV file.")
  }
  
  # Lire les données
  data <- read.table(dataset_path, header = TRUE, sep = ",")
  # Vérifier le nombre de colonnes
  if (ncol(data) != 2) {
    stop("File must have exactly two columns.")
  }
  # Vérifier les noms des colonnes
  if (!all(c("X", "Y") %in% names(data))) {
    stop("Column names must be 'X' and 'Y'.")
  }
  
  # Lire le shapefile
  shapefile <- raster::shapefile(shapefile_path)
  shapefile_sf <- sf::st_as_sf(shapefile)
  
  # Créer la carte
  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = shapefile_sf, fill = "lightblue", color = "blue", size = 2) + 
    ggplot2::geom_point(data = data, ggplot2::aes(x = X, y = Y, color = "Species"), size = 1) + 
    ggplot2::theme_minimal() + 
    ggplot2::labs(x = "Longitude", y = "Latitude") + 
    ggplot2::theme(legend.position = "none") + 
    ggplot2::theme(legend.position = c(0.8, 0.1), 
                   legend.background = ggplot2::element_rect(color = "black", fill = "white")) + 
    ggplot2::scale_color_manual(name = "Legend", 
                                values = c("orange"), 
                                labels = c("Species"))
  
  # Sauvegarder la carte
  output_plot <- file.path(output_folder, "map_plot.png")
  ggplot2::ggsave(output_plot, plot = map, width = 10, height = 8)
  print(map)
  cat("Plot saved successfully.\n")
  
  # Obtenir les coordonnées des données
  data_coords <- as.matrix(data[, c("X", "Y")])
  
  return(list(data_coords = data_coords, shapefile_path = shapefile_path, output_folder = output_folder))
}

#dataset_path <- "Datasets/xy.csv"
#shapefile_path <- "Datasets/morocco.shp"
#output_folder <- "output"

#data_list <- data_vis(dataset_path, shapefile_path, output_folder)