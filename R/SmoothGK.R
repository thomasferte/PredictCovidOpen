#' SmoothGK
#'
#' @description Smoothing function using gaussian kernel
#'
#' @param x The x vector to smooth. First value is the farthest, last value is the current value.
#' @param b The bandwidth (default is 1)
#'
#' @return The smoothed value for the current point.
#' @export
#'
#' @examples
#'
#' x <- 1:5
#' SmoothGK(x = x, b = 1)
#'
SmoothGK <- function(x, b = 1){
  vecDist <- c(rev(1:length(x)))-1

  weights <- exp(-(-vecDist)^2/2*b^2)

  wmean <- weighted.mean(x = x, w = weights)

  return(wmean)
}
