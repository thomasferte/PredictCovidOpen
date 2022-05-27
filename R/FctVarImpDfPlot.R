#' FctVarImpDfPlot
#'
#' @description Compute the optimal dataframe to plot variables importance
#'
#' @param dfImp A dataframe with all columns needed as specified by following arguments
#' @param Features Features column name
#' @param Importance Importance column name
#' @param groupVar Grouping variables column name
#' @param topN Only importance for the topN variables according to median will be computed (default is 20)
#' @param CleanFeaturesName A boolean (default is TRUE). If true, features names are passed through FctCleanFeaturesName() fonction to clean the features names.
#'
#' @return A dataframe with the columns as specified in Feautres, Importance and groupVar for the topN variables
#' @export
FctVarImpDfPlot <- function(dfImp,
                            Features = "Features",
                            Importance = "Importance",
                            groupVar = "param_FORECAST",
                            topN = 20,
                            CleanFeaturesName = TRUE){

  df_top20 <- dfImp %>%
    group_by_at(.vars = c(Features, groupVar)) %>%
    summarise(MedianImp = median(.data[[Importance]], na.rm = T)) %>%
    group_by_at(.vars = c(groupVar)) %>%
    top_n(n = 20, wt = MedianImp) %>%
    ungroup() %>%
    left_join(dfImp) %>%
    select_at(c(Features, Importance, groupVar)) %>%
    na.omit() %>%
    mutate_at(.vars = Features, .funs = FctCleanFeaturesName)

  return(df_top20)
}
