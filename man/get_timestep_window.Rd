% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_processing.R
\name{get_timestep_window}
\alias{get_timestep_window}
\title{Get timestep window for year 2-3 analysis}
\usage{
get_timestep_window(con, table_name, y0 = 2, y1 = 3)
}
\arguments{
\item{con}{DBI database connection}

\item{table_name}{Character string naming the table}

\item{y0}{Numeric start year (default: 2)}

\item{y1}{Numeric end year (default: 3)}
}
\value{
List with start, end, and data_start timesteps
}
\description{
Get timestep window for year 2-3 analysis
}
