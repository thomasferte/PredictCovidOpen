#' ForecastLm
#'
#' @description [Production] Train linear regression model on the subset of variable specified and return forecasting at given time.
#'
#' @param df The dataframe of interest
#' @param variableSubset The subset of variables.
#' @param param_FORECAST The forecast length.
#' @param param_DATE The date from which forecast is done.
#' @param delaySIDEP Delay of SIDEP data (default is 4 for historical analysis)
#' @param interactionPCR Boolean. Should an interaction between dep and P_tous_ages be added.
#'
#' @return A list of two dataframe with the Prediction and the model coefficients
#' @export
#'
#'
#' @examples
#' \dontrun{
#' library(PredictCovid)
#' ##################### import datasets ##################
#' ## CHU file
#' load(file = "extdata/COVID_ferte/dataCovidCHU.rdata")
#' ## SIDEP + DM
#' dfSidepChu <- ImportCleanSIDEPCHU(df_chu = dataCovidCHU)
#'
#' ##################### train elastic-net ##################
#' vecForecast <- 1:14
#' dfForecast <- sapply(vecForecast,
#'                      simplify = F,
#'                      function(param_FORECAST) ForecastEnet(df = dfSidepChu,
#'                                                            param_FORECAST = param_FORECAST))
#'
#' dfPredObs <- bind_rows(dfForecast) %>%
#'          rename("DATE" = "outcomeDate",
#'                 "hosp" = "Prediction") %>%
#'          mutate(STATUS = "Prediction") %>%
#'            bind_rows(dfSidepChu %>%
#'                          select(dep, DATE, hosp) %>%
#'                           mutate(STATUS = "Observed"))
#'
#' }
ForecastLm <- function(df,
                       variableSubset = c("hosp", "hosp_fct_firstDeriv7", "dep",
                                          "hosp_fct_firstDeriv7_fct_firstDeriv7",
                                          "P_tous_ages",
                                          "P_tous_ages_fct_firstDeriv7",
                                          "P_tous_ages_fct_firstDeriv7_fct_firstDeriv7",
                                          "FracP_tous_ages_fct_firstDeriv7",
                                          "FracP_tous_ages_fct_firstDeriv7_fct_firstDeriv7",
                                          "FracP_tous_ages"),
                       param_FORECAST = 7,
                       interactionPCR = F,
                       param_DATE,
                       delaySIDEP = 4){

  ### compute the outcome
  ## generate the outcome depending on the forecast time and the type of Y
  df_y <- FctComputeY(df,
                      param_Y = "hosp",
                      param_FORECAST = param_FORECAST,
                      outcome = "hosp")

  ## select the explanatory variables from the X parameter
  df_x <- FctSelectFeatures(df = df_y, param_X = "pcrhosp")

  ## separate test and train set taking into account delay between SIDEP and CHU
  ls_df <- FctTrainTestSidepDelay(df = df_x,
                                  param_DATE = param_DATE,
                                  delaySIDEP = delaySIDEP,
                                  param_VARIATION = F)

  ### train and prediction sets
  dfTrain <- ls_df$df_train$X %>%
    select(all_of(variableSubset)) %>%
    mutate(outcome = ls_df$df_train$Y)

  dfPred <- ls_df$df_test$X %>%
    select(all_of(variableSubset)) %>%
    mutate(outcome = ls_df$df_test$Y)

  ### train the model
  if(interactionPCR){
    m1_lm <- lm(formula = outcome ~ . + P_tous_ages*dep,
                data = dfTrain)

  } else {
    m1_lm <- lm(formula = outcome ~ .,
                data = dfTrain)
  }

  m1_param <- as.data.frame(confint(m1_lm)) %>%
    tibble::rownames_to_column(var = "Feature") %>%
    mutate(coef = coef(m1_lm))

  Prediction <- predict.lm(m1_lm, dfPred, interval="predict")

  dfResult <- ls_df$dfTestFull %>%
    mutate(outcomeDate = DATE + lubridate::days(param_FORECAST),
           MeanPred = Prediction[,"fit"],
           CIinf = Prediction[,"lwr"],
           CIsup = Prediction[,"upr"]) %>%
    select(dep, outcome, outcomeDate, MeanPred, CIinf, CIsup)

  return(list(Prediction = dfResult, Coef = m1_param))
}
