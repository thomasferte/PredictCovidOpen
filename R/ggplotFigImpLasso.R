#' ggplotFigImpLasso
#'
#' @description Show the lasso importance of the best model.
#'
#' @param dfPredictionsBest Best model predictions.
#' @param dflsDataGroupImp The translation between id and characteristics.
#' @param dflsDataImp The data to import.
#'
#' @return A ggplot object
#' @export
ggplotFigImpLasso <- function(dfPredictionsBest,
                              dflsDataGroupImp,
                              dflsDataImp){
  plotfigImpLasso <- dfPredictionsBest %>%
    mutate(ImportanceType = factor(Model, levels = c("PRED_RF0.5", "PRED_ENET"), labels = c("Rf - Permutation", "Lasso"))) %>%
    filter(FORECAST == 7) %>%
    select(SELECTION, OUTCOME, SPAN, Model, ImportanceType) %>%
    distinct() %>%
    left_join(dflsDataGroupImp) %>%
    left_join(dflsDataImp) %>%
    mutate(Label = paste0(OUTCOME, "_",
                          SELECTION, "_span",
                          SPAN, "_",
                          Model)) %>%
    filter(Features != "(Intercept)") %>%
    group_by(Features, Label, ImportanceType) %>%
    summarise(PrctSelect = sum(Importance != 0)/n(),
              MeanImportane = mean(Importance)) %>%
    filter(ImportanceType == "Lasso") %>%
    group_by(Label) %>%
    slice_max(PrctSelect, n = 20) %>%
    ggplot(mapping = aes(x = PrctSelect, y = reorder_within(Features, PrctSelect, Label), group = Features)) +
    geom_point() +
    facet_wrap(. ~ Label, ncol = 1, scales = "free_y") +
    scale_y_reordered() +
    labs(x = "% selected by elastic-net", y = "") +
    theme(strip.text.x = element_text(size = 6))

  return(plotfigImpLasso)
}
