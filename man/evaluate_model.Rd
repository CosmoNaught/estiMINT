% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluation.R
\name{evaluate_model}
\alias{evaluate_model}
\title{Evaluate model performance}
\usage{
evaluate_model(model_obj, X_test, y_test, model_label = "Model")
}
\arguments{
\item{model_obj}{Model object from train_xgboost or train_random_forest}

\item{X_test}{Numeric matrix of test features}

\item{y_test}{Numeric vector of test targets}

\item{model_label}{Character label for the model}
}
\value{
List with predictions and metrics data frame
}
\description{
Evaluate model performance
}
