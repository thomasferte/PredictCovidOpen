#' ggplotFeaturesManualSet
#'
#' @description Plot features of a manual selection
#'
#' @param dfDashboard The data
#' @param vecFeatures The vector of features
#'
#' @return A ggplot object
#' @export
ggplotFeaturesManualSet <- function(dfDashboard,
                                    vecFeatures){
  plotfigFeaturesVISmall <- dfDashboard %>%
    arrange(START_DATE) %>%
    mutate(outcome = lead(CHU_HOSP, 7)) %>%
    select(START_DATE, outcome, all_of(vecFeatures)) %>%
    mutate(across(where(is.numeric), .fns = scale)) %>%
    tidyr::pivot_longer(all_of(vecFeatures), names_to = "Feature") %>%
    ggplot(mapping = aes(x = START_DATE)) +
    geom_line(mapping = aes(y = value, color = "Feature")) +
    geom_line(mapping = aes(y = outcome, color = "Hospitalisations 7 days")) +
    scale_color_viridis_d(end = 0.9) +
    facet_wrap(. ~ Feature, scales = "free_y", ncol = 2) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(x = "Date", y = "scaled value", colour = "")

  return(plotfigFeaturesVISmall)
}
