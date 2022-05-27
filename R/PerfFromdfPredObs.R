#' PerfFromdfPredObs
#'
#' @description Compute the perf table from the dfPredObs obtained with ForecastLm function.
#'
#' @param dfPredObs The dataframe to be evaluated
#'
#' @return a dataframe with departement, forecast time, period of evaluation, absolute and relative errors and coverage percentage.
#' @export
PerfFromdfPredObs <- function(dfPredObs){
  result <- dfPredObs %>%
    na.omit() %>%
    mutate(coverage = (outcome <= CIsup) & (outcome >= CIinf),
           absoluteError = abs(MeanPred - outcome),
           relativeError = absoluteError/outcome) %>%
    group_by(dep, param_FORECAST) %>%
    ## perf globales
    mutate_at(.vars = c("absoluteError", "relativeError"),
              .funs = list(Overall = MedianIQR)) %>%
    mutate_at(.vars = c("coverage"),
              .funs = list(Prct_Overall = function(x) mean(x, na.rm = T))) %>%
    ## perf 2021
    filter(outcomeDate >= as.Date("2021-01-01")) %>%
    mutate_at(.vars = c("absoluteError", "relativeError"),
              .funs = list(Y2021 = MedianIQR)) %>%
    mutate_at(.vars = c("coverage"),
              .funs = list(Prct_Y2021 = function(x) mean(x, na.rm = T))) %>%
    ## perf last 2 weeks
    filter(outcomeDate >= (max(outcomeDate) - lubridate::days(14))) %>%
    mutate_at(.vars = c("absoluteError", "relativeError"),
              .funs = list(W2weeks = MedianIQR)) %>%
    mutate_at(.vars = c("coverage"),
              .funs = list(Prct_W2weeks = function(x) mean(x, na.rm = T))) %>%
    ungroup() %>%
    ## transform table to nice format
    mutate_if(.predicate = is.numeric, .funs = function(x) paste0(round(x, digits = 2))) %>%
    select(-c(outcome, outcomeDate, MeanPred, CIinf, CIsup, coverage, absoluteError, relativeError)) %>%
    distinct() %>%
    tidyr::pivot_longer(cols = absoluteError_Overall:Prct_W2weeks) %>%
    mutate(Period = gsub(pattern = ".*_", replacement = "", x = name),
           Indicator = gsub(pattern = "_.*", replacement = "", x = name)) %>%
    select(-name) %>%
    tidyr::pivot_wider(names_from = "Indicator", values_from = "value") %>%
    rename("Forecast (days)" = "param_FORECAST",
           "Absolute Error" = "absoluteError",
           "Relative Error" = "relativeError",
           "Coverage Prct" = "Prct")
  return(result)
}
