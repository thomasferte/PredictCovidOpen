#' FindBestModelbyOutcome
#'
#' @description Find the best model by outcome
#'
#' @param dfPerf The performance dataframe from Dfperformances().
#'
#' @return A dataframe with the best model by outcome
#' @export
FindBestModelbyOutcome <- function(dfPerf){
  res <- dfPerf %>%
    mutate(OUTCOME2 = as.factor(OUTCOME),
           OUTCOME2 = forcats::fct_collapse(OUTCOME,
                                            "HOSPorPrevalent" = c("Prevalent variation", "Prevalent"))) %>%
    group_by(FORECAST, OUTCOME2) %>%
    slice_min(order_by = MRE, n = 1) %>%
    select(SELECTION, OUTCOME, FORECAST, SPAN, Model)

  return(res)
}
