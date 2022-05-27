#' TabcompareModelCompromise
#'
#' @description Generate table of best models with also the bestmodelcompromise
#'
#' @param bestModelCompromise The table of best model compromise
#' @param tabBestModelsPerf The table of best model by outcome
#' @param dfBestCompromiseModelHierarchy The table of compromise value for each model
#'
#' @return A table with best model and their sum of MAE or MRE over the different outcome times
#' @export
TabcompareModelCompromise <- function(bestModelCompromise,
                                      tabBestModelsPerf,
                                      dfBestCompromiseModelHierarchy){
  tabcompareModelCompromise <- bestModelCompromise %>%
    select(SELECTION, OUTCOME, SPAN, Model) %>%
    bind_rows(tabBestModelsPerf %>%
                ungroup() %>%
                select(SELECTION, OUTCOME, SPAN, Model) %>%
                filter(OUTCOME != "Incident")) %>%
    distinct() %>%
    left_join(dfBestCompromiseModelHierarchy)

  return(tabcompareModelCompromise)
}
