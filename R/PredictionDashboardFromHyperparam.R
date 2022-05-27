#' PredictionDashboardFromHyperparam
#'
#' @description Compute predictions for CHU dashboard model depending on hyperparameters.
#'
#' @param df The dataframe
#' @param lsFeatures The list of features computed by the hyperparam_chu_dashboard.R script
#' @param SELECTION The selection parameter muste be one of the following: VI (user input), VI_7dMaxDeriv (another user input), vecVI_small (a third user input), All (all features are used), One_step (selection is done prior to 10-12-2020), Adaptative (selection is done using lasso and rf, for rf a number of features equals to lasso length of selection is used), Adaptative_3months (same as adaptative but the training of this model is used only on previous 3 months)
#' @param OUTCOME The OUTCOME must be one of HOSP or IN_HOSP
#' @param DATE The date from which prediction is made
#' @param FORECAST The forecast time.
#'
#' @return A list of two dataframes : dfPred (predictions) and dfImportance (variable importance)
#' @export
PredictionDashboardFromHyperparam <- function(df,
                                              lsFeatures,
                                              SELECTION,
                                              OUTCOME,
                                              DATE,
                                              FORECAST){

  ## step 1 : generate the outcome and transformed features
  dfOutcome <- ComputeOutcome(df = df, OUTCOME = OUTCOME, FORECAST = FORECAST) %>%
    ComputerolMeanMaxMinDeriv(df = .,
                              excludeTansf = c("START_DATE", "outcome", "outcomeDate"))

  ## step 2 : train/test
  lsTrainTest <- SeparateTrainTest(df = dfOutcome, DATE = DATE, SELECTION = SELECTION)

  xTrain <- lsTrainTest$df_train %>% select(-c(outcome, outcomeDate, START_DATE))
  yTrain <- lsTrainTest$df_train %>% pull(outcome)
  xTest <- lsTrainTest$df_test %>% select(-c(outcome, outcomeDate, START_DATE))

  ## step 3 : feature selection
  lsSelectedFeatures <- SelectedFeaturesFromHyperparam(SELECTION = SELECTION,
                                                       lsFeatures = lsFeatures,
                                                       df_train = lsTrainTest$df_train,
                                                       OUTCOME = OUTCOME,
                                                       FORECAST = FORECAST)

  ##### step 4 : train elastic-net model

  lsmodelEnet <- trainElasticNet(FeaturesLasso = lsSelectedFeatures$featuresLasso,
                                 xTrain = xTrain,
                                 xTest = xTest,
                                 yTrain = yTrain)

  ##### step 5 : train random-forest model

  lsmodelRf <- trainRf(FeaturesRf = lsSelectedFeatures$featuresRf,
                       xTrain = xTrain,
                       xTest = xTest,
                       yTrain = yTrain)

  ##### step 6 : prediction and variable importance

  dfPredictions <- MergeRfandEnetPredictions(df_test = lsTrainTest$df_test,
                                             lsmodelEnet = lsmodelEnet,
                                             lsmodelRf = lsmodelRf,
                                             OUTCOME = OUTCOME)

  ##### step 7 : return selected variables

  dfImportance <- MergeRfandEnetImportance(lsmodelEnet = lsmodelEnet,
                                           lsmodelRf = lsmodelRf)

  return(list(dfPred = dfPredictions,
              dfImportance = dfImportance))
}
