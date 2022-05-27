#' trainElasticNet
#'
#' @description Train elastic-net model with dummies for CHU_WEEKDAY
#'
#' @param FeaturesLasso The Lasso selected features
#' @param xTrain The train set
#' @param xTest The test set
#' @param yTrain The y of train set
#'
#' @return A list of model and predictions
#' @export
trainElasticNet <- function(FeaturesLasso,
                            xTrain,
                            xTest,
                            yTrain){
  # to dummy for CHU_WEEKDAY
  if(any(grepl(FeaturesLasso, pattern = "CHU_WEEKDAY$"))){
    FeaturesLasso <- c(FeaturesLasso[FeaturesLasso != "CHU_WEEKDAY"],
                       paste0("CHU_WEEKDAY_", c("lundi", "mardi", "jeudi", "vendredi", "samedi")))
  }

  xTrainFactor <- fastDummies::dummy_columns(.data = xTrain,
                                             select_columns = "CHU_WEEKDAY",
                                             remove_first_dummy = T,
                                             remove_selected_columns = T) %>%
    select(all_of(FeaturesLasso)) %>%
    as.matrix()

  xTestFactor <- fastDummies::dummy_columns(.data = xTest,
                                            select_columns = "CHU_WEEKDAY",
                                            remove_first_dummy = T,
                                            remove_selected_columns = T) %>%
    select(all_of(FeaturesLasso)) %>%
    as.matrix()


  ## train model
  modelEnet <- glmnet::cv.glmnet(x = xTrainFactor,
                                 alpha = 0.5,
                                 y = yTrain,
                                 nfolds = 10,
                                 family = "gaussian")

  predEnet <- as.numeric(predict(modelEnet, newx = xTestFactor))

  return(list(model = modelEnet,
              pred = predEnet))
}
