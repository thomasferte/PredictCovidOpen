#' BuildDfPredictionVariation
#'
#' @description Build the prediction variation dataframe from predictions and raw values (last one is used for constant predictions)
#'
#' @param dfDataPred The raw prediction dataframe
#' @param dfDashboard The raw data
#'
#' @return A cleaned prediction dataframe
#' @export
BuildDfPredictionVariation <- function(dfDataPred,
                                       dfDashboard){
  res <- dfDataPred %>%
    filter(OUTCOME %in% c("Prevalent variation", "HOSP_DERIV", "Prevalent")) %>%
    select(START_DATE, outcomeDate, SELECTION, FORECAST, OUTCOME, SPAN, outcome, PRED_ENET, PRED_RF0.5) %>%
    na.omit() %>%
    left_join(dfDashboard %>% select(START_DATE, CHU_HOSP), by = "START_DATE") %>% # add constant predictions
    mutate(across(c("PRED_ENET", "PRED_RF0.5", "outcome"), .fns = function(x) x - CHU_HOSP)) %>%
    select(outcomeDate, SELECTION, FORECAST, OUTCOME, SPAN, outcome, PRED_ENET, PRED_RF0.5) %>%
    tidyr::pivot_longer(cols = c("PRED_ENET", "PRED_RF0.5"), names_to = "Model", values_to = "Prediction") %>%
    mutate(Label = paste0(OUTCOME, "_",
                          SELECTION, "_span",
                          SPAN, "_",
                          Model))

  return(res)
}
