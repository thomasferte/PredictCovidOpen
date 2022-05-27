#' OneStepFeatureSelection
#'
#' @description Function to compute variable importance from outcome and forecast hyperparameters using lasso and random forest
#'
#' @param OUTCOME The outcome either HOSP or IN_HOSP
#' @param FORECAST The forecast time
#' @param df The dataframe of interest assuming date is stored in START_DATE column
#'
#' @return A dataframe with variable importance of RF and Lasso
#' @export
OneStepFeatureSelection <- function(OUTCOME,
                                    FORECAST,
                                    df){

  ## step 1 : generate the outcome
  dfOutcome <- ComputeOutcome(df = df, OUTCOME = OUTCOME, FORECAST = FORECAST) %>%
    na.omit()

  vecY <- dfOutcome$outcome
  dfX <- dfOutcome %>%
    select(-START_DATE, -outcome, -outcomeDate, -outcomeRef)

  ## step 2 : train model
  # lasso
  charFactorVar <- colnames(dfX)[sapply(dfX, function(x) is.factor(x)|is.character(x))]
  matX <- fastDummies::dummy_columns(.data = dfX,
                                     select_columns = charFactorVar,
                                     remove_first_dummy = T,
                                     remove_selected_columns = T) %>%
    as.matrix()

  modelLasso <- glmnet::cv.glmnet(x = matX,
                                  alpha = 1,
                                  y = vecY,
                                  nfolds = 10,
                                  family = "gaussian")
  # rf
  modelRf <- ranger::ranger(y = vecY, x = dfX, importance = "permutation")

  ## step 4 : return selected variables
  nrow1se <- FctVarImpEnet(modelLasso) %>% filter(Importance != 0) %>% nrow()

  if(nrow1se == 1){
    impEnet <- FctVarImpEnet(modelLasso, s = "lambda.min") %>% mutate(ImportanceType = "Lasso")
  } else {
    impEnet <- FctVarImpEnet(modelLasso, s = "lambda.1se") %>% mutate(ImportanceType = "Lasso")
  }

  dfImportance <- bind_rows(impEnet,
                            PredictCovid::FctVarImpHyperparam(modelRf)) %>%
    filter(Features != "(Intercept)")

  return(dfImportance)
}
