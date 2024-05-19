evaluate_models <- function(results_maxent, results_rf, results_xgboost, output_folder) {
  auc_rf <- results_rf$evaluation$AUC
  auc_maxent <- results_maxent$evaluation$AUC
  auc_xgboost <- results_xgboost$evaluation$AUC

  area_rf <- results_rf$evaluation$Predicted_Area
  area_maxent <- results_maxent$evaluation$Predicted_Area
  area_xgboost <- results_xgboost$evaluation$Predicted_Area

  # Find the best model based on AUC
  if (auc_rf > auc_maxent && auc_rf > auc_xgboost) {
    best_model <- "Random Forest"

  } else if (auc_maxent > auc_rf && auc_maxent > auc_xgboost) {
    best_model <- "Maxent"
  } else {
    best_model <- "XGBoost"
  }

  results <- data.frame(
    Model = c("Random Forest", "Maxent", "XGBoost"),
    AUC = c(auc_rf, auc_maxent, auc_xgboost),
    Area = c(area_rf, area_maxent, area_xgboost)
  )
  
  # Write results to Excel
  excel_filename <- file.path(output_folder, "model_evaluation_results.xlsx")
  openxlsx::write.xlsx(results, excel_filename, rowNames = FALSE)
  
  cat("Evaluation results saved successfully at:", excel_filename, "\n")
  cat("Best model:", best_model, "\n")
}
