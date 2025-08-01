% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot_predictions_by_cov_bin}
\alias{plot_predictions_by_cov_bin}
\title{Plot predictions by covariate bins}
\usage{
plot_predictions_by_cov_bin(
  y_true,
  predictions_list,
  df,
  covariate,
  edges,
  output_dir
)
}
\arguments{
\item{y_true}{Numeric vector of true values}

\item{predictions_list}{List of prediction vectors}

\item{df}{Data frame containing covariates}

\item{covariate}{Character name of covariate to plot}

\item{edges}{Numeric vector of bin edges}

\item{output_dir}{Character path to output directory}
}
\description{
Plot predictions by covariate bins
}
