
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

