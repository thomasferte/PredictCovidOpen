#' ggplotFigImp
#'
#' @description Show the feature importance of the best model
#'
#' @param bestModelCompromise The best model
#' @param dflsDataGroupImp The translator between ID and model characteristics.
#' @param dflsDataImp The importance
#' @param ModelType The model either "Lasso" or "Rf - Permutation"
#' @param nbFeatures The nb of features to be displayed
#'
#' @return A ggplot object
#' @export
ggplotFigImp <- function(bestModelCompromise,
                         dflsDataGroupImp,
                         dflsDataImp,
                         ModelType = "Lasso",
                         nbFeatures = 20){

  if(ModelType == "Lasso"){
    xlabel <- "% selected by elastic-net"
    measure <- "PrctSelect"
  } else if(ModelType == "Rf - Permutation"){
    xlabel <- "Mean importance"
    measure <- "MeanImportance"
  } else {
    stop("ModelType should be either 'Lasso' or 'Rf - Permutation")
  }

  dfAll7dNoSpanRawHosp <- bestModelCompromise %>%
    select(-Model) %>%
    left_join(dflsDataGroupImp) %>%
    left_join(dflsDataImp) %>%
    filter(FORECAST == 7, Features != "(Intercept)") %>%
    group_by(Features, ImportanceType) %>%
    summarise(PrctSelect = sum(Importance != 0)/n(),
              MeanImportance = mean(Importance)) %>%
    ungroup()

  plotfigImpLassoAll <- dfAll7dNoSpanRawHosp %>%
    filter(ImportanceType == ModelType) %>%
    mutate(Features = forcats::fct_reorder(Features, get(measure))) %>%
    slice_max(get(measure), n = nbFeatures) %>%
    ggplot(mapping = aes_string(y = "Features", x = measure)) +
    geom_point() +
    labs(x = xlabel, y = "") +
    theme(strip.text.x = element_text(size = 6))

  return(plotfigImpLassoAll)
}
