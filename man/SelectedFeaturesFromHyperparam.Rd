% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SelectedFeaturesFromHyperparam.R
\name{SelectedFeaturesFromHyperparam}
\alias{SelectedFeaturesFromHyperparam}
\title{SelectedFeaturesFromHyperparam}
\usage{
SelectedFeaturesFromHyperparam(
  SELECTION,
  lsFeatures,
  df_train,
  OUTCOME,
  FORECAST
)
}
\arguments{
\item{SELECTION}{The selection parameter muste be one of the following: VI (user input), VI_7dMaxDeriv (another user input), vecVI_small (a third user input), All (all features are used), One_step (selection is done prior to 10-12-2020), Adaptative (selection is done using lasso and rf, for rf a number of features equals to lasso length of selection is used), Adaptative_3months (same as adaptative but the training of this model is used only on previous 3 months)}

\item{lsFeatures}{The list of features computed by the hyperparam_chu_dashboard.R script}

\item{df_train}{The train set}

\item{OUTCOME}{The OUTCOME must be one of HOSP or IN_HOSP}

\item{FORECAST}{The forecast time.}
}
\value{
A list with random forest and lasso features selection
}
\description{
Select features from hyperparameters
}
