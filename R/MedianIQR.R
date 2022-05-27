#' MedianIQR
#'
#' @description Compute the median and the IQR, return a nice text
#'
#' @param x A numeric vector
#' @param na.rm remove NA (default is TRUE)
#'
#' @return A character string with median and IQR
#' @export
MedianIQR <- function(x, na.rm = T){
  quantX <- round(quantile(x = x, probs = c(0.25, 0.5, 0.75), na.rm = na.rm), digits = 2)

  result <- paste0(quantX[2], " [", quantX[1], " ; ", quantX[3], "]")

  return(result)
}
