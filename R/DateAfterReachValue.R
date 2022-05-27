#' DateAfterReachValue
#'
#' @description Obtain date after a feature exceed a certain threshold value.
#'
#' @param value A numeric value
#' @param df a dataframe
#' @param feature the feature column name
#' @param date the date column name
#'
#' @return A date
#' @export
DateAfterReachValue <- function(value, df, feature, date = "START_DATE"){
  result <- df %>%
    filter(get(feature) >= value) %>%
    filter(get(date) == min(get(date))) %>%
    pull(get(date))
  return(result)
}
