#' cvRf_slarTask
#'
#' @description Compute cross validated performance of randomforest model adapted to slurm curta
#'
#' @param df Dataframe
#' @param char_colDate The date column name (default is "DATE")
#' @param ntree Number of tree for randomforest (default is 1)
#' @param slar_taskid Slurm task id
#' @param param_Y a character vector of length 1 indicating the type of Y variable
#' @param param_X a character vector of length 1 indicating the X variables included
#' @param param_FORECAST a dbl vector of length 1 indicating the forecast time
#' @param param_DATE a date vetor of length 1 indicating the date of which test prediction is made
#' @param param_METEO boolean (default is FALSE). Should meteorological data be used.
#' @param param_NPI numeric (default is -1). Should NPI be used for prediction and with which delay.
#' @param delaySIDEP a numeric scalar, SIDEP delay with CHU, NULL (default) if no delay. Usually there are 4 days delay.
#' @param param_SMOOTH numeric (default is 0). Should derivatives and meanCases be calculated on a smoothed feature value.
#' @param param_VARIATION a boolean, work on variation of number (ratio) instead of raw values.
#' @param outcome The outcome of interest, default is "hosp".
#' @param variableSubset A character vector. Default is c("hosp", "hosp_fct_firstDeriv7", "dep", "Population", "P_tous_ages", "P_tous_ages_fct_firstDeriv7", "FracP_tous_ages_fct_firstDeriv7", "FracP_tous_ages", "IPTCC.mean_avg7")
#' @param varNPI A character vector. Default is c("conf1", "conf2", "ecole", "bar", "mas_clo", "mas_pub_clo", "X6pm", "X8pm", "X9pm")
#' @param RfFull boolean. Train randomforest on all variables.
#' @param EnetFull boolean. Train elastic-net on all variables.
#' @param EnetSubset boolean. Train elastic-net on subset variables.
#' @param RfSubset boolean. Train randomforest on subset variables.
#' @param EnetSubset33SIDEP boolean. Train elastic-net on subset variables with only 33 data.
#' @param EnetSubset33CHU boolean. Train elastic-net on subset variables with only CHU data.
#'
#' @return A list of two dataframe: cross validated tree predictions and variables importance
#' @import lubridate
# #' @importFrom  randomForest randomForest
# #' @importFrom ranger ranger
#' @export
cvRf_slarTask <- function(df,
                          char_colDate = "DATE",
                          slar_taskid,
                          ntree = 1,
                          param_Y = param_Y,
                          param_FORECAST = param_FORECAST,
                          param_DATE = param_DATE,
                          param_X = param_X,
                          delaySIDEP = NULL,
                          param_VARIATION = F,
                          param_METEO = F,
                          param_NPI = -1,
                          param_SMOOTH = 0,
                          outcome = "hosp",
                          RfFull = T,
                          EnetFull = T,
                          EnetSubset = T,
                          RfSubset = T,
                          EnetSubset33SIDEP = T,
                          EnetSubset33CHU = T,
                          variableSubset = c("hosp", "hosp_fct_firstDeriv7", "dep",
                                             "hosp_fct_firstDeriv7_fct_firstDeriv7",
                                             "Population", "P_tous_ages",
                                             "P_tous_ages_fct_firstDeriv7",
                                             "P_tous_ages_fct_firstDeriv7_fct_firstDeriv7",
                                             "FracP_tous_ages_fct_firstDeriv7",
                                             "FracP_tous_ages_fct_firstDeriv7_fct_firstDeriv7",
                                             "FracP_tous_ages",
                                             "IPTCC.mean_avg7"),
                          varNPI = c("conf1", "conf2", "ecole",
                                     "bar", "mas_clo", "mas_pub_clo",
                                     "X6pm", "X8pm", "X9pm")){

  ## add NPI variables to subset if NPI are used
  if(param_NPI != -1){
    variableSubset <- c(variableSubset, varNPI)
    varMarginalEffect <- varNPI
  } else{
    boolNPIcol <- colnames(df) %in% varNPI
    df <- df[,!boolNPIcol]
    varMarginalEffect <- NULL
  }

  ## generate the outcome depending on the forecast time and the type of Y
  df_y <- FctComputeY(df, param_Y = param_Y, param_FORECAST = param_FORECAST, outcome = outcome)

  ## select the explanatory variables from the X parameter
  df_x <- FctSelectFeatures(df = df_y, param_X = param_X)


  if(param_METEO){
    ## import meteo data and impute missing value using data from the past only
    dfImputedMeteo <- AddMeteoImputedMissing(weather_data_dep = PredictCovid::weather_data_dep %>% filter(date_day <= param_DATE),
                                             weather_data_reg = PredictCovid::weather_data_reg %>% filter(date_day <= param_DATE))

    df_x <- df_x %>%
      mutate(depMerge = gsub(pattern = "_.*", replacement = "", x = dep), .before = 1) %>%
      left_join(dfImputedMeteo %>% select(-reg),
                by = c("depMerge" = "dep",
                       "DATE" = "date_day")) %>%
      select(-depMerge)

  }

  ## separate test and train set taking into account delay between SIDEP and CHU
  ls_df <- FctTrainTestSidepDelay(df = df_x,
                                  param_DATE = param_DATE,
                                  delaySIDEP = delaySIDEP,
                                  param_VARIATION = param_VARIATION)

  ## initialise results
  ls_results <- list()

  ## train rf model
  if(RfFull){
    ls_rf <- FctTrainRangerQRF(yTrain = ls_df$df_train$Y,
                               xTrain = ls_df$df_train$X,
                               ntree = ntree,
                               xTest = ls_df$df_test$X,
                               fullTest = ls_df$dfTestFull,
                               varMarginalEffect = varMarginalEffect,
                               param_Y = param_Y,
                               param_X = param_X,
                               param_DATE = param_DATE,
                               param_FORECAST = param_FORECAST,
                               param_VARIATION = param_VARIATION)
    ls_results[["RfFull"]] <- ls_rf
  }


  if(EnetFull){
    ## train Elastic-net model with all variables
    EnetFull <- FctTrainEnet(yTrain = ls_df$df_train$Y,
                             xTrain = ls_df$df_train$X,
                             xTest = ls_df$df_test$X,
                             fullTest = ls_df$dfTestFull,
                             param_Y = param_Y,
                             param_X = param_X,
                             param_DATE = param_DATE,
                             param_FORECAST = param_FORECAST,
                             param_VARIATION = param_VARIATION)

    ls_results[["EnetFull"]] <- EnetFull
  }
  ## train Elastic-net model with subset of variables
  xTrainPartial <- ls_df$df_train$X[,colnames(ls_df$df_train$X) %in% variableSubset]
  xTestPartial <- ls_df$df_test$X[,colnames(ls_df$df_test$X) %in% variableSubset]


  if(EnetSubset){
    EnetSubset <- FctTrainEnet(yTrain = ls_df$df_train$Y,
                               xTrain = xTrainPartial,
                               xTest = xTestPartial,
                               fullTest = ls_df$dfTestFull,
                               param_Y = param_Y,
                               param_X = param_X,
                               param_DATE = param_DATE,
                               param_FORECAST = param_FORECAST,
                               param_VARIATION = param_VARIATION)

    ls_results[["EnetSubset"]] <- EnetSubset
  }

  if(RfSubset){
    ## train randomforest with subset of variables
    RfSubset <- FctTrainRangerQRF(yTrain = ls_df$df_train$Y,
                                  xTrain = xTrainPartial,
                                  ntree = ntree,
                                  xTest = xTestPartial,
                                  fullTest = ls_df$dfTestFull,
                                  varMarginalEffect = varMarginalEffect,
                                  param_Y = param_Y,
                                  param_X = param_X,
                                  param_DATE = param_DATE,
                                  param_FORECAST = param_FORECAST,
                                  param_VARIATION = param_VARIATION)

    ls_results[["RfSubset"]] <- RfSubset
  }
  #### Train models on subset but with only 33_SIDEP or 33_CHU in the train-test sets.
  variableSubsetLocal <- variableSubset[!variableSubset %in% c("dep", "Population")]

  ls_df33SIDEP <- FctTrainTestSidepDelay(df = df_x %>% filter(dep == "33_SIDEP"),
                                         param_DATE = param_DATE,
                                         delaySIDEP = delaySIDEP,
                                         param_VARIATION = param_VARIATION)

  ls_df33CHU <- FctTrainTestSidepDelay(df = df_x %>% filter(dep == "33_CHU"),
                                       param_DATE = param_DATE,
                                       delaySIDEP = delaySIDEP,
                                       param_VARIATION = param_VARIATION)

  ## train Elastic-net model with subset of variables on 33_SIDEP only
  if(EnetSubset33SIDEP & (length(ls_df33SIDEP$df_train$Y) != 0)){
    EnetSubset33SIDEP <- FctTrainEnet(yTrain = ls_df33SIDEP$df_train$Y,
                                      xTrain = ls_df33SIDEP$df_train$X %>% select(any_of(variableSubsetLocal)),
                                      xTest = ls_df33SIDEP$df_test$X %>% select(any_of(variableSubsetLocal)),
                                      fullTest = ls_df33SIDEP$dfTestFull,
                                      param_Y = param_Y,
                                      param_X = param_X,
                                      param_DATE = param_DATE,
                                      param_FORECAST = param_FORECAST,
                                      param_VARIATION = param_VARIATION)

    ls_results[["EnetSubset33SIDEP"]] <- EnetSubset33SIDEP
  }

  ## train Elastic-net model with subset of variables on 33_CHU only
  if(EnetSubset33CHU & (length(ls_df33CHU$df_train$Y) != 0)){
    EnetSubset33CHU <- FctTrainEnet(yTrain = ls_df33CHU$df_train$Y,
                                    xTrain = ls_df33CHU$df_train$X %>% select(any_of(variableSubsetLocal)),
                                    xTest = ls_df33CHU$df_test$X %>% select(any_of(variableSubsetLocal)),
                                    fullTest = ls_df33CHU$dfTestFull,
                                    param_Y = param_Y,
                                    param_X = param_X,
                                    param_DATE = param_DATE,
                                    param_FORECAST = param_FORECAST,
                                    param_VARIATION = param_VARIATION)

    ls_results[["EnetSubset33CHU"]] <- EnetSubset33CHU
  }


  ## merge all results
  df_varimp <- lapply(ls_results, FUN = function(x) x$df_varimp) %>%
    bind_rows(.id = "Model") %>%
    mutate(param_METEO = param_METEO,
           param_NPI = param_NPI,
           param_SMOOTH = param_SMOOTH)

  df_predictions <- lapply(ls_results, FUN = function(x) x$df_predictions) %>%
    bind_rows(.id = "Model") %>%
    mutate(param_METEO = param_METEO,
           param_NPI = param_NPI,
           param_SMOOTH = param_SMOOTH)

  return(list(df_varimp = df_varimp,
              df_predictions = df_predictions))
}
