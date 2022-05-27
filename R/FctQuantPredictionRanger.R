#' FctQuantPredictionRanger
#'
#' @description Quantile prediction from ranger dealing with param_VARIATION and HOSPHAB hyperparam
#'
#' @param rangerModel A ranger random forest model
#' @param qinf quantile inf (default = 0.025)
#' @param qsup quantile sup (default = 0.975)
#' @param dfTestX The X test
#' @param dfTestFull The full tests set for convenient output
#' @param param_Y the Y hyperparam
#' @param param_VARIATION The VARIATION hyperparam
#'
#' @return A dataframe with predictions
#' @export
FctQuantPredictionRanger <- function(rangerModel,
                                     qinf = 0.025,
                                     qsup = 0.975,
                                     dfTestX,
                                     dfTestFull,
                                     param_Y,
                                     param_VARIATION){
  ## make prediction
  rangerPredictions <- predict(object = rangerModel, data = dfTestX)
  rangerPredictionsQuant <- predict(object = rangerModel, data = dfTestX, type = "quantiles", quantiles = c(qinf, qsup))
  pred_rf <- rangerPredictions$predictions
  df_pred <- data.frame(QInf = rangerPredictionsQuant$predictions[,1],
                        Qsup = rangerPredictionsQuant$predictions[,2],
                        MeanPred = pred_rf)

  ## take into account prediction per 100000 hab
  if(param_Y == "HOSPHAB"){
    df_pred <- df_pred %>%
      mutate_all(.funs = function(x) x*dfTestX$Population/10^5)
  }

  ## take into account param_VARIATION
  if(param_VARIATION){
    df_predictions <- dfTestFull %>%
      select(dep, reg, DATE, outcomeDate, outcome, Population, hosp) %>%
      bind_cols(df_pred) %>%
      mutate(MeanPred = MeanPred + hosp,
             QInf = QInf + hosp,
             Qsup = Qsup + hosp) %>%
      select(-hosp)

  } else{
    df_predictions <- dfTestFull %>%
      select(dep, reg, DATE, outcomeDate, outcome, Population) %>%
      bind_cols(df_pred)
  }
  return(df_predictions)
}
