#' ForecastLmTime
#'
#' @description Function to compute ForecastLm on a vector of Date.
#'
#' @param Forecast The forecast time
#' @param vecSidepDate The vector of dates
#' @param dfSidep The dataframe of prediction
#'
#' @return A dataframe with the predictions.
#' @export
ForecastLmTime <- function(Forecast, vecSidepDate, dfSidep){
  pbapply::pbsapply(vecSidepDate,
                    simplify = F,
                    function(param_DATE){

                      result <- ForecastLm(df = dfSidep,
                                           param_FORECAST = Forecast,
                                           param_DATE = param_DATE,
                                           delaySIDEP = NULL)
                      return(result$Prediction)

                    }) %>%
    bind_rows() %>%
    mutate(param_FORECAST = Forecast)
}
