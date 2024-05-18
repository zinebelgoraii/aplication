# predict1.R

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
  
  # Create response vector for training data
  response <- c(rep(1, nrow(pres_train)), rep(0, nrow(backg_train)))
  
  # Extract predictor values for training data
  envtrain_presence <- raster::extract(predictors_masked, pres_train)
  envtrain_background <- raster::extract(predictors_masked, backg_train)
  
  # Combine presence and background data
  envtrain <- rbind(envtrain_presence, envtrain_background)
  
  # Create data frame with response and predictor values
  envtrain <- data.frame(response = response, envtrain)
  
  # Remove rows with missing values
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
  # Predict to raster layers
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
  
  # Predict on the test set
  response_test <- c(rep(1, nrow(pres_test)), rep(0, nrow(backg_test)))
  envtest_presence <- raster::extract(predictors_masked, pres_test)
  envtest_background <- raster::extract(predictors_masked, backg_test)
  envtest <- rbind(envtest_presence, envtest_background)
  envtest <- data.frame(response = response_test, envtest)
  envtest <- envtest[complete.cases(envtest), ]
  
  p <- envtest[envtest$response == 1, -1]
  a <- envtest[envtest$response == 0, -1]
  
  test_data <- rbind(p, a)
  test_labels <- c(rep(1, nrow(p)), rep(0, nrow(a)))
  
  predictions <- dismo::predict(rng_dws, data = test_data)$predictions[, "1"]
  
  # Calculate AUC using pROC
  roc_obj <- pROC::roc(test_labels, predictions)
  auc_value <- pROC::auc(roc_obj)
  
  # Calculate predicted area
  predicted_area <- sum(pred_rng_dws[] >= 0.5, na.rm = TRUE) * raster::res(pred_rng_dws)[1] * raster::res(pred_rng_dws)[2]
  
  # Save results to Excel
  results <- data.frame(Model = "Random Forest", AUC = auc_value, Predicted_Area = predicted_area)
  return(list(model = rng_dws, evaluation = results, prediction = pred_rng_dws))
}

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
  
  dismo::maxent()
  mx <- dismo::maxent(predictors_masked, pres_train)
  e_mx <- dismo::evaluate(pres_test, backg_test, mx, predictors_masked)
  
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

evaluate_models <- function(rf_result, maxent_result, output_folder) {
  auc_rf <- rf_result$evaluation$AUC
  auc_maxent <- maxent_result$evaluation$AUC
  best_model <- if (auc_rf > auc_maxent) "Random Forest" else "Maxent"
  
  pred_rf <- rf_result$prediction
  pred_maxent <- maxent_result$prediction
  
  area_rf <- sum(raster::values(pred_rf) > 0.5) * raster::res(pred_rf)[1] * raster::res(pred_rf)[2]
  area_maxent <- sum(raster::values(pred_maxent) > 0.5) * raster::res(pred_maxent)[1] * raster::res(pred_maxent)[2]
  
  results <- data.frame(
    Model = c("Random Forest", "Maxent"),
    AUC = c(auc_rf, auc_maxent),
    Area = c(area_rf, area_maxent)
  )
  
  openxlsx::write.xlsx(results, file.path(output_folder, "model_evaluation_results.xlsx"), rowNames = FALSE)
  
  cat("Evaluation results saved successfully at:", file.path(output_folder, "model_evaluation_results.xlsx"), "\n")
  cat("Best model:", best_model, "\n")
}

