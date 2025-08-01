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

    if (model_name == "XGBoost") {
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