#' ggplotErrorModelComparison
#'
#' @description Plot the difference of prediction of best model compared to other models
#'
#' @param dfResultsMergedPred The dataframe from which the best prediction are extracted
#' @param dfSidepLmPred The dataframe with sidep model predictions
#' @param trainTestDate The train test date
#' @param dfDashboard The raw data
#' @param dfPredresultSensitivitySidep The sensitivity analysis removing sidep data
#' @param type The type either "SIDEP" or "NoSidep"
#'
#' @return A ggplot object
#' @export
ggplotErrorModelComparison <- function(dfResultsMergedPred,
                                       dfSidepLmPred,
                                       trainTestDate,
                                       dfDashboard,
                                       dfPredresultSensitivitySidep,
                                       type){

  sidepOutcomeDate <- dfSidepLmPred %>% filter(FORECAST == 7) %>% pull(outcomeDate)

  maxdfLockdownCurfewRawPlotBest2 <- min(max(dfResultsMergedPred$outcomeDate, na.rm = T),
                                         max(sidepOutcomeDate, na.rm = T))
  mindfLockdownCurfewRawPlotBest2 <- max(min(dfResultsMergedPred$outcomeDate, na.rm = T),
                                         min(sidepOutcomeDate, na.rm = T))

  dfLockdownCurfewRawPlotBest2 <- dfLockdownCurfewRaw %>%
    mutate(BoolInclude = start < maxdfLockdownCurfewRawPlotBest2 & end > mindfLockdownCurfewRawPlotBest2) %>%
    filter(BoolInclude) %>%
    mutate(start = if_else(start < mindfLockdownCurfewRawPlotBest2, mindfLockdownCurfewRawPlotBest2, start),
           end = if_else(end > maxdfLockdownCurfewRawPlotBest2, maxdfLockdownCurfewRawPlotBest2, end),
           NPI = forcats::fct_reorder(NPI, start))

  dfPredBestNPI <- dfResultsMergedPred %>%
    filter(FORECAST == 7, START_DATE >= trainTestDate, outcomeDate <= maxdfLockdownCurfewRawPlotBest2) %>%
    select(START_DATE, outcomeDate, outcome, PRED_ENET) %>%
    rename(BestCompromise = PRED_ENET)

  if(type == "SIDEP"){

    res <- dfPredBestNPI %>%
      left_join(dfSidepLmPred %>%
                  filter(FORECAST == 7) %>%
                  select(outcomeDate, PRED_ENET) %>%
                  rename(SIDEP = PRED_ENET)) %>%
      left_join(dfDashboard %>%
                  select(START_DATE, CHU_HOSP, IPTCC.mean)) %>%
      tidyr::pivot_longer(cols = c(BestCompromise, SIDEP, CHU_HOSP)) %>%
      mutate(Error = abs(outcome - value),
             name = factor(name,
                           levels = c("BestCompromise",
                                      "SIDEP",
                                      "CHU_HOSP"),
                           labels = c("CHU Model",
                                      "SIDEP Model",
                                      "Constant"))) %>%
      na.omit() %>%
      ggplotBestvsOthers(df = ., dfNPI = dfLockdownCurfewRawPlotBest2)

  } else if(type == "NoSidep"){

    res <- dfPredBestNPI %>%
      left_join(dfPredresultSensitivitySidep %>%
                  filter(FORECAST == 7,
                         SIDEP == "No",
                         SPAN == bestModelCompromise$SPAN,
                         OUTCOME == bestModelCompromise$OUTCOME,
                         SELECTION == bestModelCompromise$SELECTION) %>%
                  mutate(outcomeDate = START_DATE + FORECAST) %>%
                  select(outcomeDate, PRED_ENET) %>%
                 rename(NO_SIDEP = PRED_ENET)) %>%
      left_join(dfDashboard %>%
                  select(START_DATE, CHU_HOSP, IPTCC.mean)) %>%
      tidyr::pivot_longer(cols = c(BestCompromise, NO_SIDEP, CHU_HOSP)) %>%
      mutate(Error = abs(outcome - value),
             name = factor(name,
                           levels = c("BestCompromise",
                                      "NO_SIDEP",
                                      "CHU_HOSP"),
                           labels = c("CHU Model",
                                      "CHU Model without Gironde PCR and hospitalisations",
                                      "Constant"))) %>%
      na.omit() %>%
      ggplotBestvsOthers(df = ., dfNPI = dfLockdownCurfewRawPlotBest2)

  }

  return(res)
}

