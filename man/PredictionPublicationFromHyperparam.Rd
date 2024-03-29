% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PredictionPublicationFromHyperparam.R
\name{PredictionPublicationFromHyperparam}
\alias{PredictionPublicationFromHyperparam}
\title{PredictionPublicationFromHyperparam}
\usage{
PredictionPublicationFromHyperparam(
  span = 7,
  model = "enet",
  df,
  features,
  forecast = 7,
  rolderiv = TRUE,
  date = as.Date("2020-10-01"),
  outcomeCol = "hosp",
  ntreeFrechet = 100,
  ncoresFrechet = 1,
  boot = FALSE,
  frechet_julia = FALSE
)
}
\arguments{
\item{span}{The span smoothing}

\item{model}{The statistical model ('enet' or 'poisson' or 'rf')}

\item{df}{The data}

\item{features}{The features to use}

\item{forecast}{The forecast time}

\item{rolderiv}{Should the ComputerolMeanMaxMinDeriv be computed}

\item{date}{The date of prediction}

\item{outcomeCol}{The outcome column}

\item{ntreeFrechet}{Nb of trees for Frechet}

\item{ncoresFrechet}{Nb of cores for Frechet}

\item{boot}{should bootstrap sampling be performed on train set}

\item{frechet_julia}{Should julia implementation of Frechet random forest be used (default is FALSE)}
}
\value{
A list of length 2 with predictions and importance.
}
\description{
Fonction to train model and obtain predictions and variable importance for publication.
}
