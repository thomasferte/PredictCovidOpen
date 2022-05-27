#' cvRfpred
#'
#' @description Compute cross validated performance of randomforest model
#'
#' @param dbl_depOfInterest Depatement of interest (default is 33)
#' @param df Dataframe
#' @param char_colDate The date column name (default is "DATE")
#' @param date_min The minimum date for learning before cross-validation (default 2020-08-01)
#' @param ntree Number of tree for randomforest (default is 1)
#'
#' @return A dataframe with cross-validated predictions
#' @export
cvRfpred <- function(dbl_depOfInterest = 33,
                     df,
                     char_colDate = "DATE",
                     date_min = as_date("2020-08-01", format = "%Y-%m-%d"),
                     ntree = 1){
  ## derive variables from user variables
  maxDate <- as_date(max(df %>% pull(char_colDate)))
  dbl_regOfInterest <- df %>%
    filter(dep == dbl_depOfInterest) %>%
    pull(reg) %>%
    unique()
  ## loop variables
  i <- 0
  df_cvpredictions <- NULL
  ## loop
  nbCycle <- as.numeric(maxDate - date_min)
  progression <- utils::txtProgressBar(min = 0,
                                       max = nbCycle,
                                       style = 3)
  while((date_min + i) < maxDate){
    date_actu <- date_min + i
    ## train set
    df_foldTrain <- df %>% filter(DATE <= date_actu, reg != dbl_regOfInterest)
    Xtrain <- as.matrix(df_foldTrain %>% select(-reg, -dep, -DATE, -forecast))
    Ytrain <- df_foldTrain %>% pull(forecast)
    ## test set
    df_foldTest <- df %>% filter(DATE == date_actu+1, dep == dbl_depOfInterest)
    Xtest <- as.matrix(df_foldTest %>% select(-reg, -dep, -DATE, -forecast))
    Ytest <- df_foldTest %>% pull(forecast)
    ## train model
    m1_rf <- randomForest(y = Ytrain,
                          x = Xtrain,
                          ntree = ntree)
    ## prediction
    dbl_predict <- predict(m1_rf, newdata = Xtest)
    df_predfold <- df_foldTest %>%
      mutate(PREDICTION = dbl_predict) %>%
      select(reg, dep, DATE, PREDICTION, forecast)
    ## save results
    df_cvpredictions <- rbind(df_cvpredictions, df_predfold)
    ## update i iteration number
    i = i + 1
    utils::setTxtProgressBar(pb = progression, value = i)
  }
  return(df_cvpredictions)
}
