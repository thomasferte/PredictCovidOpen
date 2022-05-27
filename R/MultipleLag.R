#' MultipleLag
#'
#' @description Create multiple lag functions with nice names.
#'
#' @param vecNum The numeric vector corresponging to the different lag.
#'
#' @return A list of functions
#' @export
MultipleLag <- function(vecNum = 1:14){
  lsres <- sapply(vecNum,
                  simplify = F,
                  FUN = function(num){
                    res <- function(x) lag(x = x, n = num)
                    return(res)
                  })
  names(lsres) <- paste0("lag_", vecNum)
  return(lsres)
}
