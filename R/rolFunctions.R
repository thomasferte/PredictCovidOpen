#' rolMean
#'
#' @description mean over last 6 days
#'
#' @param x feature
#'
#' @return The transfomed feature
#' @export
rolMean = function(x){
  slider::slide_dbl(.x = x,
                    .before = 6,
                    .f = mean)
}

#' rolMax
#'
#' @description max over last 6 days
#'
#' @param x feature
#'
#' @return The transfomed feature
#' @export
rolMax = function(x){
  slider::slide_dbl(.x = x,
                    .before = 6,
                    .f = max)
}

#' rolMin
#'
#' @description min over last 6 days
#'
#' @param x feature
#'
#' @return The transfomed feature
#' @export
rolMin = function(x){
  slider::slide_dbl(.x = x,
                    .before = 6,
                    .f = min)
}

#' rolDeriv
#'
#' @description Deriv over last lagDays days
#'
#' @param x feature
#' @param lagDays The deepness of the derivative
#'
#' @return The transformed feature
#' @export
rolDeriv = function(x, lagDays){
  (x-lag(x, n = (lagDays-1)))/lagDays
}
