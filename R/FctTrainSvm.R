#' FctTrainSvm
#'
#' @description Covid prediction with SVM model.
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
#' @param tuneCost Numeric vector of tuning values for cost parameter. Default is 10^c(0:2).
#' @param tuneGamma Numeric vector of tuning values for gamma parameter. Default is c(0.1, 0.2, 0.5).
#' @param tumeEpsilon Numeric vector of tuning values for epsilon parameter. Default is 10^c(0:2).
#' @param kernel The kernel type. Default is 'radial'.
#'
#' @return A list of two dataframe: the variable importance and the predictions
#' @importFrom fastDummies dummy_columns
# #' @importFrom e1071 tune.svm
#' @export
FctTrainSvm <- function(yTrain,
                        xTrain,
                        xTest,
                        fullTest,
                        param_Y,
                        param_X,
                        param_DATE,
                        param_FORECAST,
                        param_VARIATION,
                        tuneCost = 10,
                        tuneGamma = 0.1,
                        tumeEpsilon = 10^(-2),
                        kernel = "radial"){

  ## feature ingeneering for svm matrix structure => transform factor to dummies (if there is any factor)
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
  # m1_svm <- e1071::tune.svm(x = xTrainFactor,
  #                           y = yTrain,
  #                           gamma = tuneGamma,
  #                           cost = tuneCost,
  #                           epsilon = tumeEpsilon,
  #                           kernel = kernel)

  m1_svm <- e1071::svm(x = xTrainFactor,
                       y = yTrain,
                       gamma = tuneGamma,
                       cost = tuneCost,
                       epsilon = tumeEpsilon,
                       kernel = kernel)

  ## predictions -> todo
  vecPredictions <- predict(m1_svm$best.model,
                            newdata = xTestFactor)

  df_predictions <- fullTest %>%
    select(dep, reg, DATE, outcomeDate, outcome, Population) %>%
    mutate(MeanPred = vecPredictions,
           param_DATE = param_DATE,
           param_FORECAST = param_FORECAST,
           param_X = param_X,
           param_Y = param_Y,
           param_VARIATION = param_VARIATION,
           tuneGamma = m1_svm$best.parameters$gamma,
           tuneCost = m1_svm$best.parameters$cost,
           tuneEpsilon = m1_svm$best.parameters$epsilon)

  ## variable importance
  df_varimp <- NULL

  return(list(df_predictions = df_predictions,
              df_varimp = df_varimp))
}
