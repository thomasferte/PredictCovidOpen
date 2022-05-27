#' MergeRfandEnetImportance
#'
#' @description Compute features importance from elastic net and random forest
#'
#' @param lsmodelEnet The list from trainElasticNet()
#' @param lsmodelRf The list from trainRf()
#'
#' @return A dataframe with variable importance
#' @export
MergeRfandEnetImportance <- function(lsmodelEnet,
                                     lsmodelRf){

  if(is.null(lsmodelEnet)){
    dfImpEnet <- NULL
  } else {
    dfImpEnet <- PredictCovid::FctVarImpEnet(lsmodelEnet$model) %>% mutate(ImportanceType = "Lasso")
  }

  if(is.null(lsmodelRf)){
    dfImpRf <- NULL
  } else {
    dfImpRf <- PredictCovid::FctVarImpHyperparam(lsmodelRf$model)
  }

  dfImportance <- bind_rows(dfImpEnet,
                            dfImpRf)
  return(dfImportance)
}
