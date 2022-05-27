#' SeparateTrainTest
#'
#' @description Separate train and test data
#'
#' @param df The dataframe
#' @param DATE The date of split
#' @param SELECTION The selection parameter muste be one of the following: VI (user input), VI_7dMaxDeriv (another user input), vecVI_small (a third user input), All (all features are used), One_step (selection is done prior to 10-12-2020), Adaptative (selection is done using lasso and rf, for rf a number of features equals to lasso length of selection is used), Adaptative_3months (same as adaptative but the training of this model is used only on previous 3 months)
#' @param SKIP_CLUSTER A named list of two elements : the "start" and "end" dates of the cluster
#'
#' @return A list with train and test sets
#' @export
SeparateTrainTest <- function(df,
                              DATE,
                              SELECTION,
                              SKIP_CLUSTER = NULL){
  df_train <- df %>%
    filter(outcomeDate <= DATE)
  df_test <- df %>%
    filter(START_DATE == DATE)
  if(SELECTION == "Adaptative_3months"){
    df_train <- df_train %>% filter(START_DATE >= DATE-lubridate::days(90))
  }

  if(!is.null(SKIP_CLUSTER)){
    df_train <- df_train %>%
      filter(!((outcomeDate >= SKIP_CLUSTER$start) & (outcomeDate <= SKIP_CLUSTER$end)))
  }

  return(list(df_train = df_train,
              df_test = df_test))
}
