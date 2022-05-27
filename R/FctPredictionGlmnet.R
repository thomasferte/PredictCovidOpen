#' FctPredictionGlmnet
#'
#' @description Compute predictions for glmnet taking into account param_VARIATION and param_Y for prediction.
#'
#' @param modelEnet An elastic net model from cv.glmnet
#' @param xTest A test set
#' @param fullTest The full test set with dep, reg, DATE, outcomeDate, outcome, Population and hosp variables.
#' @param param_Y The param_Y hyperparameter equal to 'hosp' for raw prediction or 'HOSPHAB' for per 100 000 inhabitants prediction
#' @param param_VARIATION The param_VARIATION hyperparameter, taking into account raw prediction or variation prediction
#' @param s The s to select the right model from cv.glmnet
#'
#' @return A dataframe with the predictions and dep, reg, DATE, outcomeDate, outcome, Population and hosp columns from fullTest
#' @export
FctPredictionGlmnet <- function(modelEnet,
                                xTest,
                                fullTest,
                                param_Y,
                                param_VARIATION,
                                s = "lambda.1se"){

  ## make prediction
  enetPredictions <- predict(object = modelEnet, newx = xTest, s = s) %>% as.numeric()
  df_pred <- data.frame(MeanPred = enetPredictions)

  ## take into account prediction per 100000 hab
  if(param_Y == "HOSPHAB"){
    df_pred <- df_pred %>%
      mutate_all(.funs = function(x) x*fullTest$Population/10^5)
  }

  ## take into account param_VARIATION
  if(param_VARIATION){
    df_predictions <- fullTest %>%
      select(dep, reg, DATE, outcomeDate, outcome, Population, hosp) %>%
      bind_cols(df_pred) %>%
      mutate_at(.vars = colnames(df_pred), .funs = function(x) x + fullTest$hosp) %>%
      select(-hosp)

  } else{
    df_predictions <- fullTest %>%
      select(dep, reg, DATE, outcomeDate, outcome, Population) %>%
      bind_cols(df_pred)
  }

  return(df_predictions)

}
