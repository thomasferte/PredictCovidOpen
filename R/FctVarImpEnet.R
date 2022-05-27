#' FctVarImpEnet
#'
#' @param modelEnet An elastic net model from cv.glmnet() function
#' @param s The s used to choose the right beta values
#'
#' @return The dataframe for a given s with two columns: (i) the Features and (ii) the beta.
#' @export
FctVarImpEnet <- function(modelEnet, s = "lambda.1se"){
  matCoef <- coef(modelEnet, s = s) %>%
    as.matrix()
  colnames(matCoef) <- "Importance"
  dfResult <- matCoef %>%
    as_tibble(rownames = "Features") %>%
    mutate(ImportanceType = "Enet - Beta")
  return(dfResult)
}
