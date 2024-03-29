% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FctTrainEnet.R
\name{FctTrainEnet}
\alias{FctTrainEnet}
\title{FctTrainEnet}
\usage{
FctTrainEnet(
  yTrain,
  xTrain,
  xTest,
  fullTest,
  param_Y,
  param_X,
  param_DATE,
  param_FORECAST,
  param_VARIATION
)
}
\arguments{
\item{yTrain}{The y vector used for training}

\item{xTrain}{The x dataframe used for training}

\item{xTest}{The x dataframe used for testing}

\item{fullTest}{The full dataset for testing and reporting}

\item{param_Y}{The Y hyperparam ('hosp' or 'HOSPHAB')}

\item{param_X}{The X hyperparam ('hosp', 'pcr' or 'pcrhosp')}

\item{param_DATE}{The DATE hyperparam (a date)}

\item{param_FORECAST}{The forecast hyperparam (a double)}

\item{param_VARIATION}{The variation hyperparam (a boolean, if true, learning is done on the variation instead of raw outcome)}
}
\value{
A list of two dataframe: the variable importance and the predictions
}
\description{
Covid prediction with elastic-net model.
}
