#' SmoothAndRolMinMaxMeanRespectDate
#'
#' @description Smooth dataframe and add rolMin, rolMax, rolMean and rolDeriv features considering only data available at given DATE
#'
#' @param df Dataframe
#' @param DATE Date of availability
#' @param DATE_colum Date colum
#' @param skip_variables Variables to skip
#' @param span_days The span (default is 14) it is then converted into span for loess by dividing by number of rows in df.
#' @param rolderiv Should the ComputerolMeanMaxMinDeriv be computed
#'
#' @return A smoothed dataframe with the additional features
#' @export
SmoothAndRolMinMaxMeanRespectDate <- function(df,
                                              span_days = 14,
                                              DATE,
                                              rolderiv = TRUE,
                                              DATE_colum = "START_DATE",
                                              skip_variables = c("outcome", "outcomeDate")){
  dftemp <- df %>%
    filter(START_DATE <= DATE) %>%
    smoothDf(DATE_colum = DATE_colum,
             span_days = span_days,
             df = .,
             skip_variables = skip_variables)

  if(rolderiv){
    dfSmooth <- dftemp %>%
      ComputerolMeanMaxMinDeriv(., excludeTansf = c(DATE_colum, skip_variables))
  } else {
    dfSmooth <- dftemp
  }

  return(dfSmooth)
}
