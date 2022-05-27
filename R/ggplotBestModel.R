#' ggplotBestModel
#'
#' @description Plot a model predictions in the live prediction way.
#'
#' @param df A dataframe with model predictions
#' @param dfDashboard The raw data
#'
#' @return A ggplot object
#' @export
ggplotBestModel <- function(df, dfDashboard){
  plotBestModel <- df %>%
    ggplot(mapping = aes(x = outcomeDate, y = PRED_ENET, group = START_DATE, color = FORECAST)) +
    geom_line() +
    geom_line(data = dfDashboard,
              mapping = aes(x = START_DATE, y = CHU_HOSP),
              inherit.aes = FALSE,
              color = "black",
              size = 0.5) +
    scale_color_viridis_c(direction = -1, breaks = c(1, 7, 14)) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(x = "Date", y = "Hospitalisations", color = "Forecast (days)")

  return(plotBestModel)
}
