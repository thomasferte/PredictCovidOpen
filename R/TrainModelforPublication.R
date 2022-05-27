#' TrainModelforPublication
#'
#' @description Train model for publication and return importance and predictions
#'
#' @param lsTrainTest A list of length two with test and train sets
#' @param model a statistical model either rf, enet (linear elastic-net) or poisson (poisson elastic-net)
#' @param boot should bootstrap sampling be performed on train set
#'
#' @return A list of two dataframe, prediction and performance
#' @export
TrainModelforPublication <- function(lsTrainTest,
                                     model,
                                     boot = FALSE){

  # remove columns where 14 days ago value was constant
  selectedColumns <- lsTrainTest$df_train %>%
    filter(START_DATE < (max(START_DATE)-14)) %>%
    select(-c(outcome, outcomeDate, outcomeRef, START_DATE)) %>%
    janitor::remove_constant() %>%
    colnames()

  if(boot){
    lsTrainTest$df_train <- lsTrainTest$df_train %>% dplyr::slice_sample(prop = 1, replace = T)
  }

  xTrain <- lsTrainTest$df_train %>% select(all_of(selectedColumns))
  yTrain <- lsTrainTest$df_train %>% pull(outcome)
  xTest <- lsTrainTest$df_test %>% select(all_of(selectedColumns))

  ### step3 : train the statistical model
  if(model == "enet"){
    resMmodel <- glmnet::cv.glmnet(x = as.matrix(xTrain %>% select_if(.predicate = is.numeric)),
                                   alpha = 0.5,
                                   y = yTrain,
                                   nfolds = 10,
                                   family = "gaussian")
    dfPredictions <- data.frame(PRED = as.numeric(predict(resMmodel,
                                                          newx = as.matrix(xTest %>% select_if(.predicate = is.numeric)))))
    dfImportance <- FctVarImpEnet(resMmodel)


  } else if(model == "rf"){
    resMmodel <- ranger::ranger(y = yTrain,
                                x = xTrain,
                                importance = "permutation",
                                quantreg = T)
    dfPredictions <- predict(resMmodel,
                             data = xTest,
                             type = "quantiles",
                             quantiles = c(0.5, 0.025, 0.975))$predictions %>%
      as.data.frame(.) %>%
      rename("PRED" = "quantile= 0.5",
             "PRED_CI025" = "quantile= 0.025",
             "PRED_CI975" = "quantile= 0.975")
    dfImportance <- FctVarImpHyperparam(resMmodel)


  } else if(model == "poisson"){
    resMmodel <- glmnet::cv.glmnet(x = as.matrix(xTrain %>% select_if(.predicate = is.numeric)),
                                   alpha = 0.5,
                                   y = yTrain,
                                   nfolds = 10,
                                   family = "poisson")
    dfPredictions <- data.frame(PRED = as.numeric(predict(resMmodel,
                                                          newx = as.matrix(xTest %>% select_if(.predicate = is.numeric)),
                                                          type = "response")))
    dfImportance <- FctVarImpEnet(resMmodel)

  } else {
    stop("model should be 'enet', 'rf' or 'poisson")
  }

  ##### step 6 : prediction

  dfPredictions <- lsTrainTest$df_test %>%
    select(any_of(c("START_DATE", "outcome", "outcomeDate", "dep"))) %>%
    bind_cols(dfPredictions)

  if(is.null(dfPredictions$dep)){dfPredictions$dep <- "CHU_BDX"}

  return(list(dfPred = dfPredictions,
              dfImportance = dfImportance))
}
