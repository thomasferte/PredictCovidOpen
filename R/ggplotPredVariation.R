#' ggplotPredVariation
#'
#' @description Prediction variation of the best model
#'
#' @param dfPredictionsVariationBest dataframe of best model predictions
#' @param vecLabelsGG Vector of labels
#'
#' @return A ggplot object
#' @export
ggplotPredVariation <- function(dfPredictionsVariationBest,
                                vecLabelsGG){
  plotfigPredVariation <- dfPredictionsVariationBest %>%
    filter(OUTCOME %in% c("Prevalent variation", "Prevalent")) %>%
    ggplot(mapping = aes(x = outcomeDate, y = Prediction)) +
    geom_line(mapping = aes(color = "Prediction")) +
    geom_line(mapping = aes(y = outcome, color = "Observed")) +
    facet_wrap(FORECAST ~ Label, ncol = 2, labeller = labeller(Label = vecLabelsGG)) +
    theme_bw() +
    scale_color_viridis_d(direction = -1, end = 0.9) +
    scale_y_continuous(labels=function(x) ifelse(x > 0, paste0("+", x), x)) +
    theme(legend.position = "bottom") +
    guides(color=guide_legend(ncol = 2,byrow=TRUE),
           size = guide_legend(ncol = 2,byrow=TRUE)) +
    labs(x = "Date", y = "Variation of nb hospitalisations", color = "", size = "")

  return(plotfigPredVariation)

}
