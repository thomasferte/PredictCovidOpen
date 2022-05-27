#' FctVarImpHyperparam
#'
#' @description Clean varimp from ranger model
#'
#' @param rangerModel ranger random forest model
#'
#' @return A dataframe with variable importance
#' @export
FctVarImpHyperparam <- function(rangerModel){
  permutedImp <- rangerModel$variable.importance
  dfVarimp <- as.data.frame(permutedImp) %>%
    tibble::rownames_to_column("Features") %>%
    rename("Importance" = "permutedImp") %>%
    mutate(ImportanceType = "Rf - Permutation")

  return(dfVarimp)
}
