#' ComputerolMeanMaxMinDeriv
#'
#' @description Function to compute rolMean, rolMin, rolMax and rolDeriv on double columns of a dataset
#'
#' @param df The dataset
#' @param excludeTansf The double column not to take into account.
#' @param date_column The date column. Default is 'START_DATE'.
#'
#' @return The dataframe with the transformed features
#' @export
ComputerolMeanMaxMinDeriv <- function(df,
                                      date_column = "START_DATE",
                                      excludeTansf = "START_DATE"){
  result <- df %>%
    arrange(date_column) %>%
    mutate(across(where(is.double) & !any_of(excludeTansf),
                  list(rolMean = rolMean,
                       rolMax = rolMax,
                       rolMin = rolMin))) %>%
    mutate(across(where(is.double) & !any_of(excludeTansf),
                  list(rolDeriv3 = function(x) rolDeriv(x, 3),
                       rolDeriv7 = function(x) rolDeriv(x, 7),
                       rolDeriv10 = function(x) rolDeriv(x, 10),
                       rolDeriv14 = function(x) rolDeriv(x, 14)))) %>%
    dplyr::filter(across(.cols = -any_of(c("outcome", "outcomeDate", "outcomeRef")),
                         .fns = ~ !is.na(.x)))
  return(result)
}
