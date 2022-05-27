#' DfSidepPredictions
#'
#' @description Generate a clean sidep CHU prediction dataframe
#'
#' @param dfPred The prediction data frame.
#'
#' @return A clean sidep CHU prediction dataframe
#' @export
DfSidepPredictions <- function(dfPred){
  res <- dfPred %>%
    filter(dep == "33_CHU") %>%
    rename(FORECAST = param_FORECAST) %>%
    mutate(SELECTION = "lm_SIDEP_Model",
           OUTCOME = "Prevalent",
           SPAN = 0,
           PRED_ENET = MeanPred,
           PRED_RF0.5 = NA) %>%
    select(outcomeDate, SELECTION, FORECAST, OUTCOME, SPAN, outcome, PRED_ENET, PRED_RF0.5)

  return(res)
}
