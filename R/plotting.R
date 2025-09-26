#' @noRd
plot_obs_pred <- function(obs, pred, title, path_png, xlab = "Observed", ylab = "Predicted") {
  p <- ggplot2::ggplot(data.frame(obs = obs, pred = pred), ggplot2::aes(x = obs, y = pred)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_point(alpha = 0.35, size = 1.1,
                        position = ggplot2::position_jitter(width = 0, height = 0.0001)) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    ggplot2::theme_bw(base_size = 13)
  ggplot2::ggsave(path_png, p, width = 7.5, height = 6, dpi = 150)
  invisible(p)
}