#' FctTrainRangerQRF
#'
#' @description Function to compute ranger random forest
#'
#' @param yTrain The y train
#' @param xTrain The x train dataset
#' @param ntree  the number of trees
#' @param xTest the x test dataframe
#' @param fullTest the full x test for nice output
#' @param varMarginalEffect Vector of variable for which marginal effect will be evaluated on the train set.
#' @param param_Y the Y hyperparam
#' @param param_X the X hyperparam
#' @param param_VARIATION the variation hyperparam
#' @param param_DATE the date hyperparam
#' @param param_FORECAST the forecast hyperparam
#' @param param_VARIATION  the variation hyperparam
#'
#' @return a list of 2: variable importance and the predictions
#' @export
FctTrainRangerQRF <- function(yTrain,
                              xTrain,
                              ntree,
                              xTest,
                              fullTest,
                              varMarginalEffect = NULL,
                              param_Y,
                              param_X,
                              param_DATE,
                              param_FORECAST,
                              param_VARIATION){

  m1_rf <- ranger::ranger(x = xTrain,
                          y = yTrain,
                          quantreg = T,
                          num.trees	= ntree,
                          importance = "permutation")

  ## predictions
  dfPredictions <- FctQuantPredictionRanger(rangerModel = m1_rf,
                                            dfTestX = xTest,
                                            dfTestFull = fullTest,
                                            param_Y = param_Y,
                                            param_VARIATION = param_VARIATION) %>%
    mutate(param_DATE = param_DATE,
           param_FORECAST = param_FORECAST,
           param_X = param_X,
           param_Y = param_Y,
           param_VARIATION = param_VARIATION)

  ## variable importance
  PermutedImp <- FctVarImpHyperparam(m1_rf) %>%
    mutate(param_DATE = param_DATE,
           param_FORECAST = param_FORECAST,
           param_X = param_X,
           param_Y = param_Y,
           param_VARIATION = param_VARIATION)

  ## NPI marginal effect on the train set
  if(!is.null(varMarginalEffect)){

    vecMarginalEffect <- sapply(varMarginalEffect,
           simplify = F,
           FUN = function(feature){
             boolPreviousMeasure <- xTrain[,feature] == 1
             ## if measure did not occur reviously, return NULL
             anyPreviousMeasure <- any(boolPreviousMeasure)
             if(!anyPreviousMeasure){
               return(NA)
             } else {

               xTrain1 <- xTrain %>% filter(boolPreviousMeasure)
               xTrain0 <- xTrain1
               xTrain0[,feature] <- 0

               pred1 <- predict(m1_rf, data = xTrain1)
               pred0 <- predict(m1_rf, data = xTrain0)

               meanEffect <- mean(pred1$predictions - pred0$predictions)
             }
           }) %>%
      unlist()

    dfMarginalEffect <- data.frame(Features = names(vecMarginalEffect),
                                   Importance = vecMarginalEffect,
                                   ImportanceType = "Marginal effect of no NPI - train set",
                                   param_DATE = param_DATE,
                                   param_FORECAST = param_FORECAST,
                                   param_X = param_X,
                                   param_Y = param_Y,
                                   param_VARIATION = param_VARIATION)

    df_varimp <- bind_rows(PermutedImp, dfMarginalEffect)

  } else {
    df_varimp <- PermutedImp
  }

  return(list(df_predictions = dfPredictions,
              df_varimp = df_varimp))
}
