#' cvlm_slarTask
#'
#' @description Compute cross validated performance of linear regression model
#'
#' @param dbl_depOfInterest Depatement of interest (default is 33)
#' @param df Dataframe
#' @param char_colDate The date column name (default is "DATE")
#' @param slar_taskid Slurm task id
#'
#' @return A list of two dataframe: cross validated tree predictions and variables importance
#' @import lubridate
# #' @importFrom  randomForest randomForest
#' @importFrom stats lm
#' @export
cvlm_slarTask <- function(dbl_depOfInterest = 33,
                          df,
                          char_colDate = "DATE",
                          slar_taskid){
  ## derive variables from user variables
  maxDate <- lubridate::as_date(max(df %>% pull(char_colDate)))
  dbl_regOfInterest <- df %>%
    filter(dep == dbl_depOfInterest) %>%
    pull(reg) %>%
    unique()

  date_actu <- maxDate - slar_taskid
  ## train set
  df_foldTrain <- df %>%
    filter(DATE <= date_actu, reg != dbl_regOfInterest) %>%
    na.omit() %>%
    select(-reg, -dep, -DATE, -forecastDate)
  ## test set
  df_foldTest <- df %>%
    filter(DATE <= date_actu, reg != dbl_regOfInterest) %>%
    na.omit() %>%
    select(-reg, -dep, -DATE, -forecastDate)

  Xtest <- as.matrix(df_foldTest %>% select(-reg, -dep, -DATE, -forecast, -forecastDate))
  Ytest <- df_foldTest %>% pull(forecast)
  ## train model
  m1_lm <- lm(formula = forecast ~ ., data = df_foldTrain)
  ## prediction
  df_treepred <- predict(m1_lm, newdata = df_foldTest) %>%
    as.data.frame()
  df_predictions <- df_foldTest %>%
    select(reg, dep, DATE, forecast, forecastDate) %>%
    bind_cols(df_treepred)
  ## var importance
  df_varimp <- m1_rf$importance %>%
    as.data.frame() %>%
    add_rownames(var = "Variables")
  ## save results
  return(list(df_predictions = df_predictions,
              df_varimp = df_varimp))
}
