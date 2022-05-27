#' ggplotMultipleLag
#'
#' @description Plot multiple lag predictions
#'
#' @param vecMultipleLag The vector of multiple lag (maximum must be the forecast time)
#' @param dfDashboardRaw The raw features
#' @param dfPredictions The predictions dataframe
#'
#' @return A ggplot object
#' @export
ggplotMultipleLag <- function(vecMultipleLag,
                              dfDashboardRaw,
                              dfPredictions){

  dfLagDateHosp4 <- dfDashboardRaw %>%
    arrange(START_DATE) %>%
    select(START_DATE, CHU_HOSP) %>%
    mutate(across(.cols = CHU_HOSP,
                  .fns = MultipleLag(vecNum = vecMultipleLag)))

  vecColorSize <- length(vecMultipleLag)+1

  plotMultipleDelay4d <- dfPredictions %>%
    filter(FORECAST == max(vecMultipleLag)) %>%
    select(outcomeDate, FORECAST, Prediction) %>%
    left_join(dfLagDateHosp4,
              by = c("outcomeDate" = "START_DATE")) %>%
    tidyr::pivot_longer(cols = c("Prediction", matches("CHU_HOSP"))) %>%
    mutate(name = factor(name,
                         levels = c("CHU_HOSP", paste0("CHU_HOSP_lag_", vecMultipleLag), "Prediction"),
                         labels = c("Observed", paste0("delay ", vecMultipleLag, "d"), "Prediction"))) %>%
    ggplot(mapping = aes(x = outcomeDate, y = value, color = name, size = name)) +
    geom_line() +
    scale_color_manual(values = c(viridis::viridis(direction = -1, n = vecColorSize), "black")) +
    scale_size_manual(values = c(rep(0.5, vecColorSize), 1)) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(x = "Date", y = "Hospitalisations", color = "", size = "")

  return(plotMultipleDelay4d)
}
