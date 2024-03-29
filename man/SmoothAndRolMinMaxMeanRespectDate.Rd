% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SmoothAndRolMinMaxMeanRespectDate.R
\name{SmoothAndRolMinMaxMeanRespectDate}
\alias{SmoothAndRolMinMaxMeanRespectDate}
\title{SmoothAndRolMinMaxMeanRespectDate}
\usage{
SmoothAndRolMinMaxMeanRespectDate(
  df,
  span_days = 14,
  DATE,
  rolderiv = TRUE,
  DATE_colum = "START_DATE",
  skip_variables = c("outcome", "outcomeDate")
)
}
\arguments{
\item{df}{Dataframe}

\item{span_days}{The span (default is 14) it is then converted into span for loess by dividing by number of rows in df.}

\item{DATE}{Date of availability}

\item{rolderiv}{Should the ComputerolMeanMaxMinDeriv be computed}

\item{DATE_colum}{Date colum}

\item{skip_variables}{Variables to skip}
}
\value{
A smoothed dataframe with the additional features
}
\description{
Smooth dataframe and add rolMin, rolMax, rolMean and rolDeriv features considering only data available at given DATE
}
