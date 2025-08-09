#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom duckdb duckdb
#' @importFrom xgboost xgb.DMatrix xgb.cv xgb.train xgb.importance
#' @importFrom ranger  ranger
#' @importFrom future plan multisession
#' @importFrom future.apply future_lapply future_sapply
#' @importFrom utils   modifyList
#' @importFrom lhs maximinLHS
#' @importFrom R.utils withTimeout
#' @importFrom progressr with_progress progressor handlers
#' @importFrom ggplot2 ggplot aes geom_point geom_abline geom_smooth geom_bar
#'   scale_x_log10 scale_y_log10 labs coord_equal theme_bw theme_minimal
#'   theme element_text annotate ggsave xlim geom_text
#' @importFrom gridExtra arrangeGrob grid.arrange
## usethis namespace: end
NULL
