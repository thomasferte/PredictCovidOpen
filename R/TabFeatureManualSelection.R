#' TabFeatureManualSelection
#'
#' @description Get the manual feature selection table
#'
#' @param dfDashboard The dataframe with all features
#' @param lsFeatures The list of features
#'
#' @return A table with the feature selection
#' @export
TabFeatureManualSelection <- function(dfDashboard,
                                      lsFeatures){

  featuresAll <- colnames(dfDashboard %>% select(-START_DATE))
  dfFeaturesManual <- data.frame(Features = featuresAll,
                                 VI = featuresAll %in% lsFeatures$VI,
                                 VI_7dMax = featuresAll %in% lsFeatures$VI_7dMaxDeriv,
                                 VI_small = featuresAll %in% lsFeatures$VI_small,
                                 VI_wvv = featuresAll %in% lsFeatures$VI_add_wvv,
                                 VI_7dMax_wvv = featuresAll %in% lsFeatures$VI_7dMaxDeriv_add_wvv,
                                 VI_small_wvv = featuresAll %in% lsFeatures$VI_small_add_wvv,
                                 SIDEP = featuresAll %in% lsFeatures$SIDEP) %>%
    replace(., is.na(.), FALSE)

  return(dfFeaturesManual)
}
