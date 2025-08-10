# silence R CMD check "no visible binding" for NSE columns/pipeline
utils::globalVariables(c(
  "%>%",
  ".", "row_id", "true_value", "case_range", "model", "prediction", "error",
  "true", "pred", "bin", "year",
  "Feature", "Gain_scaled", "feature", "importance_scaled",
  "Model", "Quantile", "RMSE"
))
