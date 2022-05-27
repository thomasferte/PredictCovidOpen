#' ggplotBestPredictions
#'
#' @description Plot the predictions of a model with constant prediction for comparison.
#'
#' @param dfPredictionsBest Prediction dataframe with outcome, CONSTANT and Prediction columns
#'
#' @return A ggplot object
#' @export
ggplotBestPredictions <- function(dfPredictionsBest){
  plotBestPredictions <- dfPredictionsBest %>%
    tidyr::pivot_longer(cols = c("outcome", "CONSTANT", "Prediction")) %>%
    mutate(name = factor(name, levels = c("outcome", "CONSTANT", "Prediction"))) %>%
    ggplot(mapping = aes(x = outcomeDate, y = value, color = name)) +
    geom_line() +
    facet_wrap(FORECAST ~ Label, ncol = 2, scales = "free_y", labeller = labeller(Label = vecLabelsGG)) +
    theme_bw() +
    scale_color_viridis_d(direction = -1, end = 0.9, labels = c("Observed", "Constant", "Prediction")) +
    theme(legend.position = "bottom") +
    labs(x = "Date", y = "Nb hospitalisations", color = "", size = "")

  return(plotBestPredictions)
}
