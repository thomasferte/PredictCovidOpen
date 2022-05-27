#' BuildDfPredictions
#'
#' @description Build the prediction dataframe from predictions and raw values (last one is used for constant predictions)
#'
#' @param dfDataPred The raw prediction dataframe
#' @param dfDashboard The raw data
#'
#' @return A cleaned prediction dataframe
#' @export
BuildDfPredictions <- function(dfDataPred,
                               dfDashboard){
  res <- dfDataPred %>%
    select(START_DATE, outcomeDate, SELECTION, FORECAST, OUTCOME, SPAN, outcome, PRED_ENET, PRED_RF0.5) %>%
    na.omit() %>%
    left_join(dfDashboard %>% select(START_DATE, CHU_HOSP, IN_HOSP_in_COUNT, IN_HOSP_in_COUNT_rolMean), by = "START_DATE") %>% # add constant predictions
    mutate(CONSTANT = if_else(OUTCOME %in% c("Prevalent", "Prevalent variation", "HOSP_DERIV"), CHU_HOSP, NA_real_),
           CONSTANT = if_else(OUTCOME %in% c("IN_HOSP"), IN_HOSP_in_COUNT, CONSTANT),
           CONSTANT = if_else(OUTCOME %in% c("Incident"), IN_HOSP_in_COUNT_rolMean, CONSTANT)) %>%
    select(START_DATE, outcomeDate, SELECTION, FORECAST, OUTCOME, SPAN, outcome, CONSTANT, PRED_ENET, PRED_RF0.5) %>%
    tidyr::pivot_longer(cols = c("PRED_ENET", "PRED_RF0.5"), names_to = "Model", values_to = "Prediction") %>%
    mutate(Label = paste0(OUTCOME, "_",
                          SELECTION, "_span",
                          SPAN, "_",
                          Model))
  return(res)
}
