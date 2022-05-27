#' CovidDataFromI2b2
#'
#' @description Aggregate data from COVID_INDIC and COVID_FEATURES_SAVE_BIS i2b2 tables
#'
#' @param RqCOVID_INDIC Data from COVID_INDIC table
#' @param RqCOVID_FEATURES_SAVE_BIS Data from COVID_FEATURES_SAVE_BIS table
#'
#' @return A table with all the data
#' @export
CovidDataFromI2b2 <- function(RqCOVID_INDIC,
                              RqCOVID_FEATURES_SAVE_BIS){

  dfCleanCOVID_INDIC <- PredictCovid::CleanCOVID_INDIC(RqCOVID_INDIC = RqCOVID_INDIC)

  dfCleanCOVID_FEATURES_SAVE_BIS <- PredictCovid::CleanCOVID_FEATURES_SAVE_BIS(RqCOVID_FEATURES_SAVE_BIS = RqCOVID_FEATURES_SAVE_BIS)

  ##### merge both
  dfFullDashboard <- dfCleanCOVID_FEATURES_SAVE_BIS %>%
    left_join(dfCleanCOVID_INDIC, by = "START_DATE") %>%
    filter(START_DATE != Sys.Date())

  return(dfFullDashboard)
}
