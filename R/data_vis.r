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
library(dismo)
library(sf)
library(sp)
library(ggplot2)
library(raster)
library(geodata)

data_vis <- function(dataset_path, shapefile_path, output_folder) {
  if (!file.exists(dataset_path)) {
    stop("File does not exist.")
  }
  if (!grepl("\\.csv$", dataset_path, ignore.case = TRUE)) {
    stop("File is not a CSV file.")
  }
  
  data <- read.table(dataset_path, header = TRUE, sep = ",")
  if (ncol(data) != 2) {
    stop("File must have exactly two columns.")
  }
  if (!all(c("X", "Y") %in% names(data))) {
    stop("Column names must be 'X' and 'Y'.")
  }
  
  shapefile <- shapefile(shapefile_path)
  shapefile_sf <- st_as_sf(shapefile)
  
  map <- ggplot() +
    geom_sf(data = shapefile_sf, fill = "lightblue", color = "blue", size = 2) + 
    geom_point(data = data, aes(x = X, y = Y, color = "Species"), size = 1) + 
    theme_minimal() + 
    labs(x = "Longitude", y = "Latitude") + 
    theme(legend.position = "none") + 
    theme(legend.position = c(0.8, 0.1), 
          legend.background = element_rect(color = "black", fill = "white")) + 
    scale_color_manual(name = "Legend", 
                       values = c("orange"), 
                       labels = c("Species"))
  
  output_plot <- file.path(output_folder, "map_plot.png")
  ggsave(output_plot, plot = map, width = 10, height = 8)
  print(map)
  cat("Plot saved successfully.\n")
  
  data_coords <- coordinates(data)
  
  list(data_coords = data_coords, shapefile_path = shapefile_path, output_folder = output_folder)
}


#dataset_path <- "Datasets/xy.csv"
#shapefile_path <- "Datasets/morocco.shp"
#output_folder <- "output"

#data_list <- data_vis(dataset_path, shapefile_path, output_folder)