# === METRIC FUNCTIONS ===
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

mae <- function(actual, predicted) {
  mean(abs(actual - predicted), na.rm = TRUE)
}

# === BUILD RESULTS TABLE ===
results <- data.frame(
  Model = c(
    "LM Attendance (Region)", "LM Attendance (Trust)",
    "LM Over 4h % (Region)",  "LM Over 4h % (Trust)",
    "RF Attendance (Region)", "RF Attendance (Trust)",
    "RF Over 4h % (Region)",  "RF Over 4h % (Trust)",
    "Prophet Attendance (Region)", "Prophet Attendance (Trust)",
    "Prophet Over 4h % (Region)",  "Prophet Over 4h % (Trust)",
    "XGB Attendance (Region)", "XGB Attendance (Trust)",
    "XGB Over 4h % (Region)",  "XGB Over 4h % (Trust)"
  ),
  RMSE = c(
    rmse(lm_model_att_region_anova$AE_att_tot,   lm_attendance_region_preds),
    rmse(lm_model_att_trust_anova$AE_att_tot,    lm_attendance_trust_preds),
    rmse(lm_model_over4_region_anova$AE_per4_all, lm_over4_region_preds),
    rmse(lm_model_over4_trust_anova$AE_per4_all,  lm_over4_trust_preds),
    rmse(rf_model_att_region_full$AE_att_tot,     rf_attendance_region_preds),
    rmse(rf_model_att_trust_full$AE_att_tot,      rf_attendance_trust_preds),
    rmse(rf_model_over4_region_full$AE_over4_pct, rf_over4_region_preds),
    rmse(rf_model_over4_trust_full$AE_over4_pct,  rf_over4_trust_preds),
    rmse(prophet_fitted_values_att_region$y,      prophet_fitted_values_att_region$yhat),
    rmse(prophet_fitted_values_trust$y,           prophet_fitted_values_trust$yhat),
    rmse(prophet_fitted_values_over4_region$y,    prophet_fitted_values_over4_region$yhat),
    rmse(prophet_fitted_values_over4_trust$y,     prophet_fitted_values_over4_trust$yhat),
    rmse(xgb_data_att_region_full$AE_att_tot,     xgb_data_att_region_full$pred_xgb_att_region),
    rmse(xgb_data_att_trust_full$AE_att_tot,      xgb_data_att_trust_full$pred_xgb_att_trust),
    rmse(xgb_data_over4_region_full$AE_over4_pct, xgb_data_over4_region_full$pred_xgb_over4_region),
    rmse(xgb_data_over4_trust_full$AE_over4_pct,  xgb_data_over4_trust_full$pred_xgb_over4_trust)
  ),
  MAE = c(
    mae(lm_model_att_region_anova$AE_att_tot,   lm_attendance_region_preds),
    mae(lm_model_att_trust_anova$AE_att_tot,    lm_attendance_trust_preds),
    mae(lm_model_over4_region_anova$AE_per4_all, lm_over4_region_preds),
    mae(lm_model_over4_trust_anova$AE_per4_all,  lm_over4_trust_preds),
    mae(rf_model_att_region_full$AE_att_tot,     rf_attendance_region_preds),
    mae(rf_model_att_trust_full$AE_att_tot,      rf_attendance_trust_preds),
    mae(rf_model_over4_region_full$AE_over4_pct, rf_over4_region_preds),
    mae(rf_model_over4_trust_full$AE_over4_pct,  rf_over4_trust_preds),
    mae(prophet_fitted_values_att_region$y,      prophet_fitted_values_att_region$yhat),
    mae(prophet_fitted_values_trust$y,           prophet_fitted_values_trust$yhat),
    mae(prophet_fitted_values_over4_region$y,    prophet_fitted_values_over4_region$yhat),
    mae(prophet_fitted_values_over4_trust$y,     prophet_fitted_values_over4_trust$yhat),
    mae(xgb_data_att_region_full$AE_att_tot,     xgb_data_att_region_full$pred_xgb_att_region),
    mae(xgb_data_att_trust_full$AE_att_tot,      xgb_data_att_trust_full$pred_xgb_att_trust),
    mae(xgb_data_over4_region_full$AE_over4_pct, xgb_data_over4_region_full$pred_xgb_over4_region),
    mae(xgb_data_over4_trust_full$AE_over4_pct,  xgb_data_over4_trust_full$pred_xgb_over4_trust)
  )
)

# === Append LSTM results to the existing results table ===
results <- rbind(
  results,
  data.frame(
    Model = c(
      "LSTM Attendance (Region)",
      "LSTM Attendance (Trust)",
      "LSTM Over 4h % (Region)",
      "LSTM Over 4h % (Trust)"
    ),
    RMSE = c(
      mean(results_lstm$RMSE, na.rm = TRUE),
      mean(results_lstm_trust$RMSE, na.rm = TRUE),
      mean(results_lstm_over4$RMSE, na.rm = TRUE),
      mean(results_lstm_over4_trust$RMSE, na.rm = TRUE)
    ),
    MAE = c(
      mean(results_lstm$MAE, na.rm = TRUE),
      mean(results_lstm_trust$MAE, na.rm = TRUE),
      mean(results_lstm_over4$MAE, na.rm = TRUE),
      mean(results_lstm_over4_trust$MAE, na.rm = TRUE)
    )
  )
)

# === View final results ===
print(results)


