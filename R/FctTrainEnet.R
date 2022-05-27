#' FctTrainEnet
#'
#' @description Covid prediction with elastic-net model.
#'
#' @param yTrain The y vector used for training
#' @param xTrain The x dataframe used for training
#' @param xTest The x dataframe used for testing
#' @param fullTest The full dataset for testing and reporting
#' @param param_Y The Y hyperparam ('hosp' or 'HOSPHAB')
#' @param param_X The X hyperparam ('hosp', 'pcr' or 'pcrhosp')
#' @param param_DATE The DATE hyperparam (a date)
#' @param param_FORECAST The forecast hyperparam (a double)
#' @param param_VARIATION The variation hyperparam (a boolean, if true, learning is done on the variation instead of raw outcome)
#'
#' @return A list of two dataframe: the variable importance and the predictions
#' @importFrom fastDummies dummy_columns
#' @importFrom glmnet cv.glmnet
#' @export
FctTrainEnet <- function(yTrain,
                         xTrain,
                         xTest,
                         fullTest,
                         param_Y,
                         param_X,
                         param_DATE,
                         param_FORECAST,
                         param_VARIATION){

  ## feature ingeneering for glmnet matrix structure => transform factor to dummies (if there is any factor)
  charFactorVar <- colnames(xTrain)[sapply(xTrain, is.factor)]

  if(length(charFactorVar) != 0){
    # to dummy if there is factor
    xTrainFactor <- fastDummies::dummy_columns(.data = xTrain,
                                               select_columns = charFactorVar,
                                               remove_first_dummy = T,
                                               remove_selected_columns = T) %>%
      as.matrix()

    xTestFactor <- fastDummies::dummy_columns(.data = xTest,
                                              select_columns = charFactorVar,
                                              remove_first_dummy = T,
                                              remove_selected_columns = T) %>%
      as.matrix()
  } else {
    # if no factor simply transform to matrix
    xTrainFactor <- as.matrix(xTrain)
    xTestFactor <- as.matrix(xTest)
  }


  ## train model
  m1_enet <- glmnet::cv.glmnet(x = xTrainFactor,
                               alpha = 0.5,
                               y = yTrain,
                               nfolds = 10,
                               family = "gaussian")

  ## predictions -> todo
  dfPredictions <- FctPredictionGlmnet(modelEnet = m1_enet,
                                       xTest = xTestFactor,
                                       fullTest = fullTest,
                                       param_Y = param_Y,
                                       param_VARIATION = param_VARIATION) %>%
    mutate(param_DATE = param_DATE,
           param_FORECAST = param_FORECAST,
           param_X = param_X,
           param_Y = param_Y,
           param_VARIATION = param_VARIATION)

  ## variable importance
  PermutedImp <- FctVarImpEnet(modelEnet = m1_enet) %>%
    mutate(param_DATE = param_DATE,
           param_FORECAST = param_FORECAST,
           param_X = param_X,
           param_Y = param_Y,
           param_VARIATION = param_VARIATION)

  return(list(df_predictions = dfPredictions,
              df_varimp = PermutedImp))
}
