#' DfperformancesSensitivitySIDEP
#'
#' @description Compute the performance dataframe with MAE and MRE. Sensitivity analysis on SIDEP
#'
#' @param df The dataframe to compute performance
#'
#' @return A dataframe with MAE and MRE
#' @export
DfperformancesSensitivitySIDEP <- function(df){
  res <- df %>%
    select(outcomeDate, SELECTION, FORECAST, OUTCOME, SPAN, outcome, SIDEP, PRED_ENET, PRED_RF0.5) %>%
    tidyr::pivot_longer(cols = c("PRED_ENET", "PRED_RF0.5"), names_to = "Model", values_to = "Prediction") %>%
    mutate(diff = Prediction - outcome,
           relativeDiff = diff/outcome) %>%
    group_by(SELECTION, OUTCOME, FORECAST, SPAN, SIDEP, Model) %>%
    summarise(MAE = mean(abs(diff), na.rm = T),
              MRE = mean(abs(relativeDiff), na.rm = T)) %>%
    arrange(OUTCOME, FORECAST, SELECTION, SPAN) %>%
    ungroup() %>%
    na.omit()

  return(res)
}
