% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MergeRfandEnetImportance.R
\name{MergeRfandEnetImportance}
\alias{MergeRfandEnetImportance}
\title{MergeRfandEnetImportance}
\usage{
MergeRfandEnetImportance(lsmodelEnet, lsmodelRf)
}
\arguments{
\item{lsmodelEnet}{The list from trainElasticNet()}

\item{lsmodelRf}{The list from trainRf()}
}
\value{
A dataframe with variable importance
}
\description{
Compute features importance from elastic net and random forest
}
