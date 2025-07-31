# Load the package
library(testthat)
library(estiMINT)

# Mock dbGetQuery if needed
if (!exists("mock_dbGetQuery")) {
  mock_dbGetQuery <- function(con, query) {
    list(min_ts = 100)
  }
}
