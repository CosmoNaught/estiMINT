#' Create scatter plots colored by prevalence rank
#'
#' @param y_true Numeric vector of true values
#' @param predictions_list List of prediction vectors
#' @param prev_rank Numeric vector of prevalence ranks
#' @param output_dir Character path to output directory
#' @export
plot_predictions_by_prev_rank <- function(y_true, predictions_list,
                                          prev_rank, output_dir) {

  limits_raw <- range(c(y_true, unlist(predictions_list)))
  limits_raw[limits_raw <= 0] <- min(limits_raw[limits_raw > 0])
  limits_exp <- limits_raw * c(0.95, 1.05)

  plots <- list()
  for (model_name in names(predictions_list)) {
    df <- data.frame(
      true  = y_true,
      pred  = predictions_list[[model_name]],
      rank  = factor(prev_rank)
    )

    plots[[model_name]] <-
      ggplot(df, aes(true, pred, colour = rank)) +
      geom_point(alpha = .7, size = 2) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
      scale_x_log10(limits = limits_exp) +
      scale_y_log10(limits = limits_exp) +
      labs(title  = paste(model_name, "- Obs vs Pred (colour = prevalence rank)"),
           colour = "Rank\nwithin\nparameter",
           x = "Observed initial EIR (log10)",
           y = "Predicted initial EIR (log10)") +
      coord_equal() +
      theme_bw() +
      theme(plot.title   = element_text(face = "bold"),
            aspect.ratio = 1)
  }

  combined <- gridExtra::arrangeGrob(grobs = plots, ncol = 2)
  ggsave(file.path(output_dir, "predictions_scatter_by_prev_rank.png"),
         combined, width = 16, height = 8, dpi = 300)
}

#' Create combined prediction scatter plots
#'
#' @param y_true Numeric vector of true values
#' @param predictions_list List of prediction vectors
#' @param output_dir Character path to output directory
#' @export
plot_predictions_combined <- function(y_true, predictions_list, output_dir) {

  limits_raw <- range(c(y_true, unlist(predictions_list)))
  limits_raw[limits_raw <= 0] <- min(limits_raw[limits_raw > 0])
  limits_exp <- limits_raw * c(0.95, 1.05)

  plots <- list()
  for (model_name in names(predictions_list)) {
    df <- data.frame(true = y_true,
                     pred = predictions_list[[model_name]])

    r2   <- 1 - sum((df$true - df$pred)^2) /
                 sum((df$true - mean(df$true))^2)
    rmse <- sqrt(mean((df$true - df$pred)^2))
    mae  <- mean(abs(df$true - df$pred))

    point_color <- if (grepl("XGBoost", model_name)) "#2166AC" else "#1B7837"

    plots[[model_name]] <-
      ggplot(df, aes(true, pred)) +
      geom_point(alpha = .6, colour = point_color, size = 2) +
      geom_abline(intercept = 0, slope = 1,
                  colour = "red", linetype = "dashed", linewidth = 1) +
      scale_x_log10(limits = limits_exp) +
      scale_y_log10(limits = limits_exp) +
      labs(title = paste(model_name, "- Observed vs Predicted (log-log)"),
           x = "Observed initial EIR (log10)",
           y = "Predicted initial EIR (log10)") +
      annotate("text", x = limits_exp[1] * 1.1, y = limits_exp[2] / 1.1,
               label = sprintf("R^2 = %.3f\nRMSE = %.3f\nMAE = %.3f",
                               r2, rmse, mae),
               hjust = 0, vjust = 1, size = 3.5, fontface = "italic") +
      coord_equal() +
      theme_minimal() +
      theme(plot.title   = element_text(size = 14, face = "bold"),
            axis.title   = element_text(size = 12),
            aspect.ratio = 1)
  }

  if (length(plots) > 1) {
    combined <- gridExtra::arrangeGrob(grobs = plots, ncol = 2)
    ggsave(file.path(output_dir, "predictions_scatter_combined.png"),
           combined, width = 16, height = 8, dpi = 300)
  } else {
    ggsave(file.path(output_dir, "predictions_scatter_combined.png"),
           plots[[1]], width = 8, height = 8, dpi = 300)
  }
}

#' Create feature importance plots
#'
#' @param importance_list List of importance data frames
#' @param output_dir Character path to output directory
#' @param top_n Integer number of top features to display (default: 15)
#' @export
plot_feature_importance_combined <- function(importance_list, output_dir, top_n = 15) {

  plots <- list()
  for (model_name in names(importance_list)) {
    imp <- importance_list[[model_name]]

    if (model_name == "XGBoost" || grepl("XGBoost", model_name)) {
      df <- imp[1:min(top_n, nrow(imp)), ]
      df$Feature      <- factor(df$Feature,
                                levels = rev(df$Feature))
      df$Gain_scaled  <- df$Gain / max(df$Gain) * 100

      p <- ggplot(df, aes(x = Gain_scaled, y = Feature)) +
        geom_bar(stat = "identity", fill = "#2166AC", alpha = .8) +
        geom_text(aes(label = sprintf("%.1f%%", Gain_scaled)),
                  hjust = -0.1, size = 3) +
        labs(title = paste(model_name, "Feature Importance"),
             x = "Relative Importance (%)", y = "") +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, face = "bold")) +
        xlim(0, 110)
    } else {
      df <- imp[1:min(top_n, nrow(imp)), ]
      df$feature            <- factor(df$feature,
                                      levels = rev(df$feature))
      df$importance_scaled  <- df$importance /
                               max(df$importance) * 100

      p <- ggplot(df, aes(x = importance_scaled, y = feature)) +
        geom_bar(stat = "identity", fill = "#1B7837", alpha = .8) +
        geom_text(aes(label = sprintf("%.1f%%", importance_scaled)),
                  hjust = -0.1, size = 3) +
        labs(title = paste(model_name, "Feature Importance"),
             x = "Relative Importance (%)", y = "") +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, face = "bold")) +
        xlim(0, 110)
    }

    plots[[model_name]] <- p
  }

  combined <- do.call(grid.arrange, c(plots, ncol = 2))
  ggsave(file.path(output_dir,
                   "feature_importance_combined.png"),
         combined, width = 16, height = 8, dpi = 300)
}

#' Plot predictions by covariate bins
#'
#' @param y_true Numeric vector of true values
#' @param predictions_list List of prediction vectors
#' @param df Data frame containing covariates
#' @param covariate Character name of covariate to plot
#' @param edges Numeric vector of bin edges
#' @param output_dir Character path to output directory
#' @export
plot_predictions_by_cov_bin <- function(y_true, predictions_list, df,
                                        covariate, edges, output_dir) {

  lim_raw <- range(c(y_true, unlist(predictions_list)))
  lim_raw[lim_raw <= 0] <- min(lim_raw[lim_raw > 0])
  lim     <- lim_raw * c(0.95, 1.05)

  plots <- list()
  for (model_name in names(predictions_list)) {
    df_plot <- data.frame(
      true  = y_true,
      pred  = predictions_list[[model_name]],
      bin   = bin_variable(df[[covariate]], edges)
    )

    plots[[model_name]] <-
      ggplot(df_plot, aes(true, pred, colour = bin)) +
      geom_point(alpha = .7, size = 2) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
      scale_x_log10(limits = lim) +
      scale_y_log10(limits = lim) +
      labs(
        title  = sprintf("%s - Obs vs Pred (coloured by %s)", model_name, covariate),
        colour = covariate,
        x      = "Observed initial EIR (log10)",
        y      = "Predicted initial EIR (log10)"
      ) +
      coord_equal() +
      theme_bw() +
      theme(plot.title   = element_text(face = "bold"),
            aspect.ratio = 1)
  }

  combined <- gridExtra::arrangeGrob(grobs = plots, ncol = 2)
  ggsave(file.path(output_dir,
                   sprintf("predictions_scatter_by_%s.png", covariate)),
         combined, width = 16, height = 8, dpi = 300)
}

#' Batch create covariate bin plots
#'
#' @param y_true Numeric vector of true values
#' @param predictions_list List of prediction vectors
#' @param df Data frame containing covariates
#' @param covariates Character vector of covariate names
#' @param bin_edges List of bin edges for each covariate
#' @param output_dir Character path to output directory
#' @export
batch_cov_bin_plots <- function(y_true, predictions_list, df,
                                covariates, bin_edges, output_dir) {

  for (cov in covariates) {
    if (!cov %in% names(bin_edges))
      stop(sprintf("No bin edges supplied for '%s'", cov))

    plot_predictions_by_cov_bin(
      y_true          = y_true,
      predictions_list= predictions_list,
      df              = df,
      covariate       = cov,
      edges           = bin_edges[[cov]],
      output_dir      = output_dir
    )
  }
}

#' Create scatter plots for case predictions by year
#'
#' @param y_true Numeric vector of true case values
#' @param predictions_list List of prediction vectors
#' @param years Numeric vector of years
#' @param output_dir Character path to output directory
#' @export
plot_case_predictions_by_year <- function(y_true, predictions_list,
                                          years, output_dir) {

  # Use linear scale for cases
  limits <- range(c(y_true, unlist(predictions_list))) * c(0.95, 1.05)
  
  # Ensure lower limit is at least 0
  limits[1] <- max(0, limits[1])

  plots <- list()
  for (model_name in names(predictions_list)) {
    df <- data.frame(
      true  = y_true,
      pred  = predictions_list[[model_name]],
      year  = factor(years)
    )

    plots[[model_name]] <-
      ggplot(df, aes(true, pred, colour = year)) +
      geom_point(alpha = .7, size = 2) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
      ggplot2::scale_x_continuous(limits = limits) +
      ggplot2::scale_y_continuous(limits = limits) +
      labs(title  = paste(model_name, "- Obs vs Pred Cases/1000 (colour = year)"),
           colour = "Year",
           x = "Observed cases per 1000",
           y = "Predicted cases per 1000") +
      coord_equal() +
      theme_bw() +
      theme(plot.title   = element_text(face = "bold"),
            aspect.ratio = 1)
  }

  combined <- gridExtra::arrangeGrob(grobs = plots, ncol = 2)
  ggsave(file.path(output_dir, "case_predictions_scatter_by_year.png"),
         combined, width = 16, height = 8, dpi = 300)
}

#' Create combined prediction scatter plots for cases
#'
#' @param y_true Numeric vector of true case values
#' @param predictions_list List of prediction vectors
#' @param output_dir Character path to output directory
#' @export
plot_case_predictions_combined <- function(y_true, predictions_list, output_dir) {

  # Use linear scale for cases
  limits <- range(c(y_true, unlist(predictions_list))) * c(0.95, 1.05)
  
  # Ensure lower limit is at least 0
  limits[1] <- max(0, limits[1])

  plots <- list()
  for (model_name in names(predictions_list)) {
    df <- data.frame(true = y_true,
                     pred = predictions_list[[model_name]])

    r2   <- 1 - sum((df$true - df$pred)^2) /
                 sum((df$true - mean(df$true))^2)
    rmse <- sqrt(mean((df$true - df$pred)^2))
    mae  <- mean(abs(df$true - df$pred))

    point_color <- if (grepl("XGBoost", model_name)) "#2166AC" else "#1B7837"

    plots[[model_name]] <-
      ggplot(df, aes(true, pred)) +
      geom_point(alpha = .6, colour = point_color, size = 2) +
      geom_abline(intercept = 0, slope = 1,
                  colour = "red", linetype = "dashed", linewidth = 1) +
      ggplot2::scale_x_continuous(limits = limits) +
      ggplot2::scale_y_continuous(limits = limits) +
      labs(title = paste(model_name, "- Observed vs Predicted Cases/1000"),
           x = "Observed cases per 1000",
           y = "Predicted cases per 1000") +
      annotate("text", x = limits[1] + 0.05 * diff(limits), 
               y = limits[2] - 0.05 * diff(limits),
               label = sprintf("R^2 = %.3f\nRMSE = %.3f\nMAE = %.3f",
                               r2, rmse, mae),
               hjust = 0, vjust = 1, size = 3.5, fontface = "italic") +
      coord_equal() +
      theme_minimal() +
      theme(plot.title   = element_text(size = 14, face = "bold"),
            axis.title   = element_text(size = 12),
            aspect.ratio = 1)
  }

  if (length(plots) > 1) {
    combined <- gridExtra::arrangeGrob(grobs = plots, ncol = 2)
    ggsave(file.path(output_dir, "case_predictions_scatter_combined.png"),
           combined, width = 16, height = 8, dpi = 300)
  } else {
    ggsave(file.path(output_dir, "case_predictions_scatter_combined.png"),
           plots[[1]], width = 8, height = 8, dpi = 300)
  }
}

#' Plot case predictions by covariate bins with linear scale
#'
#' @param y_true Numeric vector of true case values
#' @param predictions_list List of prediction vectors
#' @param df Data frame containing covariates
#' @param covariate Character name of covariate to plot
#' @param edges Numeric vector of bin edges
#' @param output_dir Character path to output directory
#' @export
plot_case_predictions_by_cov_bin <- function(y_true, predictions_list, df,
                                             covariate, edges, output_dir) {

  # Use linear scale for cases
  limits <- range(c(y_true, unlist(predictions_list))) * c(0.95, 1.05)
  limits[1] <- max(0, limits[1])

  plots <- list()
  for (model_name in names(predictions_list)) {
    df_plot <- data.frame(
      true  = y_true,
      pred  = predictions_list[[model_name]],
      bin   = bin_variable(df[[covariate]], edges)
    )

    plots[[model_name]] <-
      ggplot(df_plot, aes(true, pred, colour = bin)) +
      geom_point(alpha = .7, size = 2) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
      ggplot2::scale_x_continuous(limits = limits) +
      ggplot2::scale_y_continuous(limits = limits) +
      labs(
        title  = sprintf("%s - Obs vs Pred Cases/1000 (coloured by %s)", model_name, covariate),
        colour = covariate,
        x      = "Observed cases per 1000",
        y      = "Predicted cases per 1000"
      ) +
      coord_equal() +
      theme_bw() +
      theme(plot.title   = element_text(face = "bold"),
            aspect.ratio = 1)
  }

  combined <- gridExtra::arrangeGrob(grobs = plots, ncol = 2)
  ggsave(file.path(output_dir,
                   sprintf("case_predictions_scatter_by_%s.png", covariate)),
         combined, width = 16, height = 8, dpi = 300)
}

#' Plot stratified performance comparison
#'
#' @param metrics_df Data frame with stratified metrics
#' @param output_dir Character path to output directory
#' @export
plot_stratified_performance <- function(metrics_df, output_dir) {
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  
  # Extract RMSE columns for different quantiles
  rmse_cols <- grep("^RMSE_[0-9]", names(metrics_df), value = TRUE)
  
  if (length(rmse_cols) > 0) {
    # Reshape for plotting
    plot_data <- metrics_df %>%
      select(Model, all_of(rmse_cols)) %>%
      pivot_longer(cols = all_of(rmse_cols), 
                   names_to = "Quantile", 
                   values_to = "RMSE") %>%
      mutate(Quantile = gsub("RMSE_", "", Quantile))
    
    # Create bar plot
    p <- ggplot(plot_data, aes(x = Quantile, y = RMSE, fill = Model)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
      labs(title = "Stratified RMSE by Case Quantiles",
           x = "Case Quantile Range",
           y = "RMSE") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = c("XGBoost-Cases" = "#2166AC", 
                                   "RandomForest-Cases" = "#1B7837"))
    
    ggsave(file.path(output_dir, "stratified_performance.png"),
           p, width = 10, height = 6, dpi = 300)
  }
}

#' Generate diagnostic plots for case predictions
#'
#' @param predictions_df Data frame from compare_case_predictions
#' @param output_dir Character path to output directory
#' @export  
plot_case_diagnostics <- function(predictions_df, output_dir) {
  library(ggplot2)
  library(tidyr)
  
  if ("true_value" %in% names(predictions_df)) {
    # Reshape for plotting
    pred_cols <- grep("_pred$", names(predictions_df), value = TRUE)
    model_names <- gsub("_pred$", "", pred_cols)
    
    plot_data <- predictions_df %>%
      select(row_id, true_value, case_range, all_of(pred_cols)) %>%
      pivot_longer(cols = all_of(pred_cols),
                   names_to = "model",
                   values_to = "prediction") %>%
      mutate(model = gsub("_pred$", "", model))
    
    # Scatter plot by case range
    p1 <- ggplot(plot_data, aes(x = true_value, y = prediction, color = model)) +
      geom_point(alpha = 0.5) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      facet_wrap(~case_range, scales = "free") +
      labs(title = "Predictions by Case Range",
           x = "True Cases/1000",
           y = "Predicted Cases/1000") +
      theme_minimal() +
      scale_color_manual(values = c("xgboost_cases" = "#2166AC",
                                   "rf_cases" = "#1B7837"))
    
    ggsave(file.path(output_dir, "case_diagnostics_by_range.png"),
           p1, width = 12, height = 8, dpi = 300)
    
    # Error distribution plot
    error_data <- predictions_df %>%
      select(row_id, case_range, all_of(grep("_error$", names(.), value = TRUE))) %>%
      pivot_longer(cols = -c(row_id, case_range),
                   names_to = "model",
                   values_to = "error") %>%
      mutate(model = gsub("_error$", "", model))
    
    p2 <- ggplot(error_data, aes(x = case_range, y = error, fill = model)) +
      geom_boxplot(alpha = 0.7) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Prediction Errors by Case Range",
           x = "Case Range (cases/1000)",
           y = "Prediction Error") +
      theme_minimal() +
      scale_fill_manual(values = c("xgboost_cases" = "#2166AC",
                                  "rf_cases" = "#1B7837"))
    
    ggsave(file.path(output_dir, "case_error_distribution.png"),
           p2, width = 10, height = 6, dpi = 300)
  }
}