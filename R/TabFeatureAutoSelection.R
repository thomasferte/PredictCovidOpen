#' TabFeatureAutoSelection
#'
#' @description Generate table of automated feature selection.
#'
#' @param dflsDataGroupImp Feature importance from ls_resultsCurtadf_groupVarimp.rds file.
#' @param dflsDataImp Feature importance from ls_resultsCurtadf_varimp.rds file.
#'
#' @return A table with the selected features.
#' @export
TabFeatureAutoSelection <- function(dflsDataGroupImp,
                                    dflsDataImp){
  varselectionauto <- dflsDataGroupImp %>%
    filter(SELECTION %in% c("One_step", "One_step_add_wvv"),
           ImportanceType == "Lasso") %>%
    left_join(dflsDataImp) %>%
    filter(Features != "(Intercept)",
           Importance != 0) %>%
    select(Features, SELECTION, OUTCOME, FORECAST, SPAN) %>%
    distinct() %>%
    group_by(SELECTION, OUTCOME, FORECAST, SPAN) %>%
    summarise(nbFeatures = n())

  return(varselectionauto)
}
