prediction_futur_mx <- function(model, input_file, predicteurs_path, shapefile_path, output_folder)
  {
  model <- model$model
  
  # Charger le shapefile (raster package)
  shapefile <- raster::shapefile(shapefile_path)
  ext_shapefile <- raster::extent(shapefile)
  cat("Chargement des fichiers raster et shapefile.\n")
  
  # Charger les prédicteurs masqués (raster package)
  predictors_masked <- raster::brick(input_file)
  
  cat("Parcourir les fichiers prédicteurs.\n")
  
  # Lire les noms de fichiers dans le dossier (base package)
  file_names <- list.files(predicteurs_path, pattern = "\\.tif$", full.names = TRUE)
  
  cat("Créer un dataframe des fichiers prédicteurs.\n")
  
  # Extraire le nom du modèle, du scénario et l'année pour chaque fichier (base package)
  model_scenario_year_info <- sapply(file_names, function(file) {
    # Extraire le nom de fichier sans le chemin (base package)
    file_name <- basename(file)
    # Extraire le modèle, le scénario et l'année en utilisant des expressions régulières (base package)
    model <- gsub("wc2\\.1_2.5m_bioc_(.*?)_ssp.*?_(\\d{4}-\\d{4})\\.tif", "\\1", file_name)
    scenario <- gsub("wc2\\.1_2.5m_bioc_.*?_(ssp.*?)_(\\d{4}-\\d{4})\\.tif", "\\1", file_name)
    year <- gsub("wc2\\.1_2.5m_bioc_.*?_ssp.*?_(\\d{4}-\\d{4})\\.tif", "\\1", file_name)
    # Retourner le modèle, le scénario et l'année sous forme de vecteur (base package)
    c(model = model, scenario = scenario, year = year)
  })
  
  # Convertir en data frame pour une meilleure lisibilité (base package)
  model_scenario_year_df <- data.frame(t(model_scenario_year_info))
  
  # Ajouter une colonne pour le chemin complet des fichiers (base package)
  model_scenario_year_df$full_path <- file_names
  
  # Extraire les valeurs uniques de la colonne "scenario" (base package)
  unique_scenarios <- unique(model_scenario_year_df$scenario)
  
  # Pour chaque valeur unique de scénario, effectuer les opérations raster et enregistrer les résultats
  for (scenario in unique_scenarios) {
    # Créer le dossier de sortie spécifique au scénario si nécessaire (base package)
    scenario_output_folder <- file.path(output_folder, scenario)
    if (!file.exists(scenario_output_folder)) {
      dir.create(scenario_output_folder, recursive = TRUE)
    }
    
    # Filtrer les fichiers correspondant au scénario actuel (base package)
    scenario_files <- model_scenario_year_df$full_path[model_scenario_year_df$scenario == scenario]
    
    # Pour chaque fichier correspondant au scénario actuel, effectuer les opérations raster
    for (file_path in scenario_files) {
      # Charger le raster (raster package)
      if (file.exists(file_path)) {
        spatraster1 <- raster::brick(file_path)
      } else {
        print(paste("Le fichier", file_path, "n'existe pas."))
        next
      }
      # Obtenir les noms des couches de predictors_masked (raster package)
      layer_names <- names(predictors_masked)
      
      # Affecter les noms des couches de predictors_masked à spatraster1 (raster package)
      names(spatraster1) <- layer_names
      
      # Masquer le raster en fonction du shapefile (raster package)
      spatraster1 <- raster::crop(spatraster1, raster::extent(shapefile))
      spatraster1 <- raster::mask(spatraster1, shapefile)
      
      p_mx <- dismo::predict(spatraster1, model)
      
      # Enregistrer le fichier prédit dans le dossier de sortie spécifique au scénario (terra package)
      output_file <- file.path(scenario_output_folder, gsub(".*wc2\\.1_2.5m_bioc_", "", basename(file_path)))
      terra::writeRaster(p_mx, filename = output_file, format = "GTiff", overwrite = TRUE)
    }
  }
  
  # Parcourir chaque scénario
  for (scenario in unique_scenarios) {
    scenario_path <- file.path(output_folder, scenario)
    
    # Vérifier si le dossier du scénario existe (base package)
    if (!file.exists(scenario_path)) {
      print(paste("Le dossier", scenario_path, "n'existe pas."))
      next
    }
    
    # Créer un dossier "moyenne" dans le dossier du scénario (base package)
    moyenne_path <- file.path(scenario_path, "moyenne")
    if (!file.exists(moyenne_path)) {
      dir.create(moyenne_path, recursive = TRUE)
    }
    
    # Lire les noms de fichiers dans le dossier du scénario (base package)
    file_names <- list.files(scenario_path, pattern = "\\.tif$", full.names = TRUE)
    
    # Extraire les années uniques des fichiers (base package)
    year_info <- sapply(file_names, function(file) {
      file_name <- basename(file)
      year <- gsub(".*_(\\d{4}-\\d{4})\\.tif$", "\\1", file_name)
      return(year)
    })
    unique_years <- unique(year_info)
    
    # Parcourir chaque année unique
    for (year in unique_years) {
      # Filtrer les fichiers correspondant à l'année actuelle (base package)
      year_files <- file_names[year_info == year]
      
      # Charger les rasters de l'année actuelle (raster package)
      raster_list <- lapply(year_files, raster::raster)
      
      # Calculer la moyenne des rasters pour l'année actuelle (raster package, base package)
      mean_raster <- raster::calc(raster::stack(raster_list), base::mean)
      
      # Enregistrer le raster moyen pour l'année actuelle dans un nouveau fichier tif (raster package)
      mean_output_file <- file.path(moyenne_path, paste("moyenne_", year, ".tif", sep = ""))
      raster::writeRaster(mean_raster, filename = mean_output_file, format = "GTiff", overwrite = TRUE)
    }
  }
  
}