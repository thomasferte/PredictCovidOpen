#' FindBestModel
#'
#' @description Find the best model for a given forecast time
#'
#' @param dfPerf The performance dataframe
#' @param forecast The forecast time
#' @param vecOutcome The vector of outcome considered
#'
#' @return The label of the best model
#' @export
FindBestModel <- function(dfPerf, forecast = 7, vecOutcome = c("Prevalent variation", "HOSP_DERIV", "Prevalent")){
  res <- dfPerf %>%
    filter(FORECAST == forecast, OUTCOME %in% vecOutcome) %>%
    slice_min(MRE, n = 1) %>%
    mutate(Label = paste0(OUTCOME, "_", SELECTION, "_span", SPAN, "_", Model)) %>%
    pull(Label)

  return(res)
}
