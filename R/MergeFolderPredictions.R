#' MergeFolderPredictions
#'
#' @description Merge folder PredictCovid Data
#'
#' @param folder The folder path
#'
#' @return A list of predictions and features importance
#' @export
MergeFolderPredictions <- function(folder){
  lsFolder <- PredVarimpFromFolder(folder = folder)
  df_pred <- lsFolder$df_pred
  df_varimp <- lsFolder$df_varimp

  # split df_varimp to lower file size
  colGroup <- colnames(df_varimp %>% select(-c(Features, Importance)))

  dfGroup <- df_varimp %>% select(all_of(colGroup)) %>%
    distinct() %>%
    tibble::rowid_to_column(var = "ID_Param")

  df_varimpLight <- df_varimp %>%
    right_join(dfGroup, by = colGroup) %>%
    select(-all_of(colGroup))

  res <- list(df_pred = df_pred,
              df_varimp = df_varimpLight,
              df_groupVarimp = dfGroup)

  return(res)
}
