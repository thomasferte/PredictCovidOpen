#' ggplotBestFeatures
#'
#' @description Plot the best features of the best model
#'
#' @param bestModelCompromise The best model
#' @param dflsDataGroupImp The translator of ID and model characteristics
#' @param dflsDataImp The variable importance
#' @param dfDashboardRaw The raw data
#'
#' @return A ggplot object
#' @export
ggplotBestFeatures <- function(bestModelCompromise,
                               dflsDataGroupImp,
                               dflsDataImp,
                               dfDashboardRaw){

  vecBestFeatures <- bestModelCompromise %>%
    mutate(ImportanceType = if_else(Model == "PRED_ENET", "Lasso", "Rf - Permutation"),
           FORECAST = 7) %>%
    select(SELECTION, OUTCOME, SPAN, FORECAST, ImportanceType) %>%
    left_join(dflsDataGroupImp) %>%
    left_join(dflsDataImp) %>%
    filter(Features != "(Intercept)",
           !grepl(Features, pattern = "CHU_WEEKDAY")) %>%
    group_by(Features) %>%
    summarise(PctKeep = sum(Importance != 0)/n()) %>%
    arrange(-PctKeep) %>%
    slice(1:20) %>%
    pull(Features)

  plotbestfeaturesBest <- dfDashboardRaw %>%
    arrange(START_DATE) %>%
    SmoothAndRolMinMaxMeanRespectDate(df = .,
                                      span_days = 21,
                                      DATE = max(dfDashboardRaw$START_DATE),
                                      DATE_colum = "START_DATE",
                                      skip_variables = NULL) %>%
    left_join(dfDashboardRaw %>%
                arrange(START_DATE) %>%
                mutate(outcome = lead(CHU_HOSP, 7)) %>%
                select(START_DATE, outcome)) %>%
    select(START_DATE, outcome, all_of(vecBestFeatures)) %>%
    mutate(across(where(is.numeric), .fns = scale)) %>%
    tidyr::pivot_longer(all_of(vecBestFeatures), names_to = "Feature") %>%
    ggplot(mapping = aes(x = START_DATE)) +
    geom_line(mapping = aes(y = value, color = "Feature")) +
    geom_line(mapping = aes(y = outcome, color = "Hospitalisations 7 days")) +
    scale_color_viridis_d(end = 0.9) +
    facet_wrap(. ~ Feature, scales = "free_y", ncol = 2) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(x = "Date", y = "scaled value", colour = "")

  return(plotbestfeaturesBest)
}
