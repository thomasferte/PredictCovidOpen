#' DfperformancesSensitivityCluster
#'
#' @description Compute the performance dataframe with MAE and MRE. Sensitivity analysis on SIDEP
#'
#' @param df The dataframe to compute performance
#' @param dfSidepLmPred The SIDEP data
#' @param dfSkipCluster The dataframe without cluster
#'
#' @return A dataframe with MAE and MRE
#' @export
DfperformancesSensitivityCluster <- function(df,
                                             dfSkipCluster,
                                             dfSidepLmPred){
  res <- df %>%
    bind_rows(dfSkipCluster) %>%
    select(outcomeDate, SELECTION, FORECAST, OUTCOME, SPAN, SKIP_CLUSTER, outcome, PRED_ENET, PRED_RF0.5) %>%
    mutate(SKIP_CLUSTER = if_else(is.na(SKIP_CLUSTER), FALSE, SKIP_CLUSTER)) %>%
    bind_rows(dfSidepLmPred) %>%
    filter(outcomeDate > lsFeatures$lsDateCluster$end) %>%
    tidyr::pivot_longer(cols = c("PRED_ENET", "PRED_RF0.5"), names_to = "Model", values_to = "Prediction") %>%
    mutate(diff = Prediction - outcome,
           relativeDiff = diff/outcome) %>%
    group_by(SELECTION, OUTCOME, FORECAST, SPAN, SKIP_CLUSTER, Model) %>%
    summarise(MAE = mean(abs(diff), na.rm = T),
              MRE = mean(abs(relativeDiff), na.rm = T)) %>%
    arrange(OUTCOME, FORECAST, SELECTION, SKIP_CLUSTER, SPAN) %>%
    ungroup() %>%
    na.omit()

  return(res)
}
