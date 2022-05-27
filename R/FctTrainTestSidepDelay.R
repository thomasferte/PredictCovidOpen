#' FctTrainTestSidepDelay
#'
#' @description Function to generate train and test sets
#'
#' @param df Dataframe of interest
#' @param param_DATE The date of interest
#' @param param_VARIATION A boolean, should the model be trained on raw value or on the delta
#' @param delaySIDEP The SIDEP delay compared to CHU
#'
#' @return A list with train and test sets
#' @export
FctTrainTestSidepDelay <- function(df,
                                   param_DATE,
                                   delaySIDEP,
                                   param_VARIATION){
  if(is.null(delaySIDEP)){
    df_train <- df %>%
      filter(outcomeDate <= param_DATE) %>%
      na.omit()
    df_test <- df %>%
      filter(DATE == param_DATE)

  } else {
    df_train <- df %>%
      filter(outcomeDate <= param_DATE,
             grepl(dep, pattern = "CHU") | outcomeDate <= param_DATE - lubridate::days(delaySIDEP)) %>%
      na.omit()

    df_test <- df %>%
      filter((grepl(dep, pattern = "CHU") & DATE == param_DATE) |
               (grepl(dep, pattern = "SIDEP") & DATE == param_DATE - lubridate::days(delaySIDEP)))
  }

  # remove test rows when NA in prediction variables
  bool_row_test <- apply(df_test %>% select(-c(outcome, outcomeDate, outcomeModel )), MARGIN = 1, FUN = anyNA)
  df_test <- df_test %>% filter(!bool_row_test)

  ls_df <- lapply(X = list(df_train = df_train,
                           df_test = df_test),
                  function(x){
                    X <- x %>% select(-c(outcome, outcomeModel, outcomeDate, DATE)) %>% as.data.frame()
                    Y <- x %>% pull(outcomeModel)
                    if(param_VARIATION) Y <- x$outcome - x$hosp
                    return(list(X=X,
                                Y=Y))
                  })
  ls_df[["dfTestFull"]] <- df_test
  return(ls_df)
}
