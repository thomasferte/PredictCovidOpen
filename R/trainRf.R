#' trainRf
#'
#' @description Train randomForest model
#'
#' @param FeaturesRf The RF selected features
#' @param xTrain The train set
#' @param xTest The test set
#' @param yTrain The y of train set
#'
#' @return A list of model and predictions
#' @export
trainRf <- function(FeaturesRf,
                    xTrain,
                    xTest,
                    yTrain){
  modelRf <- ranger::ranger(y = yTrain,
                            x = xTrain %>% select(all_of(FeaturesRf)),
                            importance = "permutation",
                            quantreg = T)
  predRf <- predict(modelRf, data = xTest, type = "quantiles", quantiles = c(0.5, 0.025, 0.975))

  return(list(model = modelRf,
              pred = predRf))
}
