#' FctComputeY
#'
#' @description Function to compute the forecast Y with a param_FORECAST distance
#'
#' @param df A dataframe with 'dep' column the department ID and 'DATE' column, the DATE column
#' @param param_Y The Y of interest
#' @param param_FORECAST The forecast distance
#' @param outcome The outcome of interest, default is "hosp".
#'
#' @return Add the outcome, the outcomeModel (different if HOSPHAB is used) and the outcomeDate
#' @export
FctComputeY <- function(df,
                        param_Y,
                        param_FORECAST,
                        outcome = "hosp"){
  dfResult <- df %>%
    group_by(dep) %>%
    arrange(dep, DATE) %>%
    mutate(outcomeModel = lead(x = get(param_Y), n = param_FORECAST),
           outcome = lead(x = get(outcome), n = param_FORECAST),
           outcomeDate = lead(x = DATE, n = param_FORECAST)) %>%
    ungroup() %>%
    select(-HOSPHAB)

  return(dfResult)
}
