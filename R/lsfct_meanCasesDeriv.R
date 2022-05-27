#' ls_fctmeanCasesDeriv
#'
#' @description Build a list of function to get first derivative and mean number of cases in the past vec_lagTimepoints.
#'
#' @param vec_lagTimepoints A vector of lag time points (default is c(1, 3, 7, 14))
#'
#' @return A list of function for past cumulating mean Cases and first derivative.
#' @export
ls_fctmeanCasesDeriv <- function(vec_lagTimepoints = c(1, 3, 7, 14)){
  ls_fctMeanCases <- sapply(vec_lagTimepoints,
                            simplify = F,
                            function(lagtime) function(x) slide_dbl(x, .before = lagtime, .f = mean))
  names(ls_fctMeanCases) <- paste0("fct_meanCases", vec_lagTimepoints)

  ls_fctFirstDeriv <- sapply(vec_lagTimepoints,
                             simplify = F,
                             function(lagtime) function(x) PredictCovid::fct_firstDeriv(x = x, lagtime = lagtime))
  names(ls_fctFirstDeriv) <- paste0("fct_firstDeriv", vec_lagTimepoints)

  return(c(ls_fctMeanCases, ls_fctFirstDeriv))
}



