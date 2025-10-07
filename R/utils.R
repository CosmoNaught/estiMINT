#' @noRd
ts <- function(...) {
  cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n")
  flush.console()
  invisible(NULL)
}

#' @noRd
r2 <- function(y, yhat) {
  ssr <- sum((y - yhat)^2)
  1 - ssr / sum((y - mean(y))^2)
}

#' @noRd
rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))

#' @noRd
mse <- function(y, yhat) mean((y - yhat)^2)

#' @noRd
mae <- function(y, yhat) mean(abs(y - yhat))

#' @noRd
median_ae <- function(y, yhat) median(abs(y - yhat))

#' @noRd
mae_rel <- function(y, yhat) median(abs(yhat - y) / pmax(1, y))

#' @noRd
rmsle <- function(y, yhat) sqrt(mean((log1p(yhat) - log1p(y))^2))

#' @noRd
safe_div <- function(num, den, eps = 1e-12) num / pmax(eps, den)

#' @noRd
smape <- function(y, yhat, eps = 1e-12) mean(2 * abs(yhat - y) / pmax(eps, abs(y) + abs(yhat)))

#' @noRd
fit_qmap_w <- function(pred_raw, obs_raw, ngrid = 1024, round_digits = 8) {
  keep <- is.finite(pred_raw) & is.finite(obs_raw)
  x <- pred_raw[keep]; y <- obs_raw[keep]

  o1 <- order(x); x1 <- x[o1]
  F1 <- (seq_along(x1) - 0.5) / length(x1)

  y_key <- signif(y, round_digits)
  y_tab <- tapply(y, y_key, function(v) length(v))
  y_u   <- as.numeric(names(y_tab))
  w_u   <- as.numeric(y_tab)

  o2 <- order(y_u); y2 <- y_u[o2]; w2 <- w_u[o2]
  F2 <- cumsum(w2) / sum(w2)

  q  <- seq(0, 1, length.out = ngrid)
  xq <- approx(F1, x1, xout = q, rule = 2)$y
  yq <- approx(F2, y2, xout = q, rule = 2)$y

  list(kind = "qmap", xq = xq, yq = yq)
}

#' @noRd
predict_qmap_w <- function(newx_raw, cal) {
  approx(cal$xq, cal$yq, xout = newx_raw, rule = 2, ties = "ordered")$y
}

#' @noRd
scale_pos <- function(obs, pred) {
  a <- sum(obs * pred) / sum(pred^2)
  if (!is.finite(a) || a <= 0) a <- 1.0
  a
}

#' @noRd
.find_installed_model <- function() {
  cand <- c(
    system.file("extdata", "eir_model", "estiMINT_model.rds", package = "estiMINT"),
    system.file("extdata", "estiMINT_model.rds", package = "estiMINT"),
    system.file("estiMINT_model.rds", package = "estiMINT")
  )
  cand <- cand[nzchar(cand) & file.exists(cand)]
  if (length(cand)) cand[[1]] else NULL
}

#' @noRd
.resolve_model_file <- function(dir_or_file) {
  if (file.exists(dir_or_file) && !dir.exists(dir_or_file)) return(dir_or_file)
  d <- normalizePath(dir_or_file, mustWork = TRUE)
  candidates <- c(
    file.path(d, "estiMINT_model.rds"),
    file.path(d, "eir_model", "estiMINT_model.rds")
  )
  for (p in candidates) if (file.exists(p)) return(p)
  hit <- list.files(d, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
  hit <- hit[basename(hit) == "estiMINT_model.rds"]
  if (length(hit)) return(hit[[1]])
  stop("Could not find 'estiMINT_model.rds' under: ", d)
}
