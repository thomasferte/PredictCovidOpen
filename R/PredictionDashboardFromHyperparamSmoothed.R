#' PredictionDashboardFromHyperparamSmoothed
#'
#' @description Compute predictions for CHU dashboard model depending on hyperparameters.
#'
#' @param df The dataframe
#' @param lsFeatures The list of features computed by the hyperparam_chu_dashboard.R script
#' @param SELECTION The selection parameter muste be one of the following: VI (user input), VI_7dMaxDeriv (another user input), vecVI_small (a third user input), All (all features are used), One_step (selection is done prior to 10-12-2020), Adaptative (selection is done using lasso and rf, for rf a number of features equals to lasso length of selection is used), Adaptative_3months (same as adaptative but the training of this model is used only on previous 3 months)
#' @param OUTCOME The OUTCOME must be one of HOSP or IN_HOSP
#' @param DATE The date from which prediction is made
#' @param FORECAST The forecast time.
#' @param span_days The span (default is 14) it is then converted into span for loess by dividing by number of rows in df.
#' @param MODEL A character vector deciding which models are computed (defaut is ENET and RF).
#' @param SKIP_CLUSTER A named list of two elements : the "start" and "end" dates of the cluster
#' @param BOOT Should sampling be performed on train set ? (default is FALSE)
#'
#' @return A list of two dataframes : dfPred (predictions) and dfImportance (variable importance)
#' @export
PredictionDashboardFromHyperparamSmoothed <- function(df,
                                                      span_days = 14,
                                                      lsFeatures,
                                                      SELECTION,
                                                      OUTCOME,
                                                      DATE,
                                                      FORECAST,
                                                      BOOT = FALSE,
                                                      MODEL = c("ENET", "RF"),
                                                      SKIP_CLUSTER = NULL){
  if(!("ENET" %in% MODEL | "RF" %in% MODEL)) stop("MODEL shoulbe : 'ENET', 'RF' or c('ENET','RF')")
  if(span_days == 0){
    ## step 1 : generate the outcome and transformed features
    dfOutcome <- ComputeOutcome(df = df, OUTCOME = OUTCOME, FORECAST = FORECAST) %>%
      ComputerolMeanMaxMinDeriv(df = .,
                                excludeTansf = c("START_DATE", "outcome", "outcomeDate", "outcomeRef",
                                                 lsFeatures$VaccinDate$features,
                                                 lsFeatures$VariantsDate$features))

    ## step 2 : train/test
    lsTrainTest <- SeparateTrainTest(df = dfOutcome, DATE = DATE, SELECTION = SELECTION, SKIP_CLUSTER = SKIP_CLUSTER)
  } else {
    ## step 1 : generate the outcome
    lsTrainTest <- ComputeOutcomeRef(df = df,
                                     OUTCOME = OUTCOME,
                                     FORECAST = FORECAST) %>%
      SmoothAndRolMinMaxMeanRespectDate(df = .,
                                        span_days = span_days,
                                        DATE = DATE,
                                        DATE_colum = "START_DATE",
                                        skip_variables = c("outcomeRef", "outcomeDate",
                                                           lsFeatures$VaccinDate$features,
                                                           lsFeatures$VariantsDate$features)) %>%
      ComputeOutcomeCustom(df = .,
                           OUTCOME = OUTCOME,
                           FORECAST = FORECAST) %>%
      SeparateTrainTest(df = .,
                        DATE = DATE,
                        SELECTION = SELECTION,
                        SKIP_CLUSTER = SKIP_CLUSTER)
  }


  xTrain <- lsTrainTest$df_train %>% select(-c(outcome, outcomeDate, outcomeRef, START_DATE))
  yTrain <- lsTrainTest$df_train %>% pull(outcome)
  xTest <- lsTrainTest$df_test %>% select(-c(outcome, outcomeDate, outcomeRef, START_DATE))

  if(BOOT){
    rowXtrain <- nrow(xTrain)
    bootRows <- sample(x = 1:rowXtrain, size = rowXtrain, replace = TRUE)
    xTrain <- xTrain[bootRows,]
    yTrain <- yTrain[bootRows]
  }

  ## step 3 : feature selection
  lsSelectedFeatures <- SelectedFeaturesFromHyperparam(SELECTION = SELECTION,
                                                       lsFeatures = lsFeatures,
                                                       df_train = lsTrainTest$df_train,
                                                       OUTCOME = OUTCOME,
                                                       FORECAST = FORECAST)

  ##### step 4 : train elastic-net model
  if("ENET" %in% MODEL){
    lsmodelEnet <- trainElasticNet(FeaturesLasso = lsSelectedFeatures$featuresLasso,
                                   xTrain = xTrain,
                                   xTest = xTest,
                                   yTrain = yTrain)
  } else {
    lsmodelEnet <- NULL
  }

  ##### step 5 : train random-forest model
  if("RF" %in% MODEL){
    lsmodelRf <- trainRf(FeaturesRf = lsSelectedFeatures$featuresRf,
                         xTrain = xTrain,
                         xTest = xTest,
                         yTrain = yTrain)
  } else {
    lsmodelRf <- NULL
  }

  ##### step 6 : prediction and variable importance

  dfPredictions <- MergeRfandEnetPredictions(df_test = lsTrainTest$df_test,
                                             lsmodelEnet = lsmodelEnet,
                                             lsmodelRf = lsmodelRf,
                                             OUTCOME = OUTCOME,
                                             FORECAST = FORECAST)

  ##### step 7 : return selected variables

  dfImportance <- MergeRfandEnetImportance(lsmodelEnet = lsmodelEnet,
                                           lsmodelRf = lsmodelRf)

  return(list(dfPred = dfPredictions,
              dfImportance = dfImportance))
}
