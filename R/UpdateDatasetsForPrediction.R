#' UpdateDatasetsForPrediction
#'
#' @description Update data for CHU predictions
#'
#' @param dfVariantsPast Past dataframe of variants with gompertz imputation.
#' @param skip_variants Should the variants be skipped (default is TRUE because variants data collection changed).
#' @param saveData Should the data be saved.
#' @param RqCOVID_INDIC Data from COVID_INDIC table
#' @param RqCOVID_FEATURES_SAVE_BIS Data from COVID_FEATURES_SAVE_BIS table
#'
#' @return A list of dataframe
#' @export
UpdateDatasetsForPrediction <- function(dfVariantsPast = readRDS(file = "extdata/dfVariantsImputed.rds"),
                                        skip_variants = TRUE,
                                        saveData = FALSE,
                                        RqCOVID_INDIC,
                                        RqCOVID_FEATURES_SAVE_BIS){

  # i2b2 data
  dfFullDashboard <- CovidDataFromI2b2(RqCOVID_INDIC = RqCOVID_INDIC,
                                       RqCOVID_FEATURES_SAVE_BIS = RqCOVID_FEATURES_SAVE_BIS)

  # variants data
  if(skip_variants){
    dfVariants <- NULL
  } else {
    dfVariants <- ImportGompertzImputeVariants(dfVariantsImputed = dfVariantsPast)
  }

  # vaccin data
  dfVaccination <- ImportCleanVaccination()

  # weather data
  weather_data_dep33 <- weather_data_from_NOAA(Regions_or_Dept_stations_pop = Dept_stations_pop[Dept_stations_pop$code_insee == 33,],
                                               years = 2020:2021,
                                               n.cores = 1)

  # merge data
  lsDfDashboard <- MergeDashboardVaccinVariantsWeather(dfFullDashboard = dfFullDashboard,
                                                       dfVariants = dfVariants,
                                                       dfVaccination = dfVaccination,
                                                       weather_data_dep33 = weather_data_dep33)

  # save data if specified
  if(saveData){
    saveRDS(dfFullDashboard, file = "extdata/COVID_ferte/dfFullDashboard.rds")

    if(!skip_variants){
      saveRDS(dfVariants, file = "extdata/dfVariantsImputed.rds")
    }

    saveRDS(dfVaccination, file = "extdata/dfVaccination.rds")
    save(weather_data_dep33, file = "data/weather_data_dep33.rdata")
    saveRDS(lsDfDashboard$dfDashboard, file = "extdata/chu_specific_model/chu_dashboard/dfDashboard.rds")
    saveRDS(lsDfDashboard$dfDashboardAll, file = "extdata/chu_specific_model/chu_dashboard/dfDashboardAll.rds")
    saveRDS(lsDfDashboard$dfDashboardRaw, file = "extdata/chu_specific_model/chu_dashboard/dfDashboardRaw.rds")
    saveRDS(lsDfDashboard$dfDashboardRawAll, file = "extdata/chu_specific_model/chu_dashboard/dfDashboardRawAll.rds")
  }

  return(list(lsDfDashboard = lsDfDashboard,
              weather_data_dep33 = weather_data_dep33,
              dfVaccination = dfVaccination,
              dfVariants = dfVariants,
              dfFullDashboard = dfFullDashboard))

}
