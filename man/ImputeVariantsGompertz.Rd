% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ImputeVariantsGompertz.R
\name{ImputeVariantsGompertz}
\alias{ImputeVariantsGompertz}
\title{ImputeVariantsGompertz}
\usage{
ImputeVariantsGompertz(dfVariantsRaw, datePlateauVariants)
}
\arguments{
\item{dfVariantsRaw}{The raw variants dataframe}

\item{datePlateauVariants}{The max date used for imputation}
}
\value{
A dataset with the imputed values.
}
\description{
Impute missing variants data with a gompertz law.
}
