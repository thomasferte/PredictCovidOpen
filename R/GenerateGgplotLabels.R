#' GenerateGgplotLabels
#'
#' @description Clean labels for ggplot graph
#'
#' @param vecLabels A vector of labels to be cleaned
#'
#' @return A cleaned named vector of labels
#' @export
GenerateGgplotLabels <- function(vecLabels){
  vecLabelsRaw <- unique(vecLabels)
  vecLabelsClean <- vecLabelsRaw %>%
    gsub(pattern = "IN_HOSP", replacement = "Incident") %>%
    gsub(pattern = "_in_COUNT_rolMean", replacement = " smooth") %>%
    gsub(pattern = "HOSP_DERIV", replacement = "Prevalent linear approx") %>%
    gsub(pattern = "Prevalent variation", replacement = "Prevalent") %>%
    gsub(pattern = "PRED_ENET", replacement = "Enet") %>%
    gsub(pattern = "PRED_RF0.5", replacement = "RF")
  vecLabelsGG <- vecLabelsClean
  names(vecLabelsGG) <- vecLabelsRaw

  return(vecLabelsGG)
}
