#' MergeDashboardVaccinVariantsWeather
#'
#' @description Merge Dashboard, Vaccin, Variants and Weather
#'
#' @param dfFullDashboard The Dashboard data
#' @param dfVariants_dep33 The variants data
#' @param dfVaccination_dep33 The vaccine data
#' @param weather_data_dep33 The weather data
#' @param vecExcludeTransf Vector of features for which feature engineering should not be performed
#'
#' @return A list of dataframe
#' @export
MergeDashboardVaccinVariantsWeather <- function(dfFullDashboard,
                                                dfVariants_dep33,
                                                dfVaccination_dep33,
                                                weather_data_dep33 = weather_data_dep33,
                                                vecExcludeTransf = c("START_DATE",
                                                                     "n_cum_dose1_tous_ages",
                                                                     "n_cum_dose1_18_59",
                                                                     "n_cum_dose1_60_80_PLUS",
                                                                     "Pct_Variants")
){
  dfDashboardRaw <- dfFullDashboard %>%
    select(-c(CHU_POPULATION, GIRONDE_POPULATION, CHU_TIME, GIRONDE_TIME, GIRONDE_WEEKDAY, CHU_HOSPHAB, GIRONDE_HOSPHAB)) %>%
    janitor::remove_constant() %>%
    mutate_if(.predicate = is.character, .funs = as.factor) %>%
    # group age categories
    GroupDashboardPCRAge()

  ##### Variants
  if(!is.null(dfVariants_dep33)){
    delayVariants <- as.numeric(max(dfFullDashboard$START_DATE) - max(dfVariants_dep33$DATE))

    if(delayVariants > 20){
      warning("Beware, variants delay is really important, are you sure you want to use this data ?")
    }

    dfVariantsPreMerged <- dfVariants_dep33 %>%
      ungroup() %>%
      filter(dep == "33") %>%
      mutate(START_DATE = DATE + delayVariants) %>%
      select(START_DATE, Majority_variant)
  }

  ### Vaccination
  delayVaccine <- as.numeric(max(dfFullDashboard$START_DATE) - max(dfVaccination_dep33$DATE))

  if(delayVaccine > 20){
    warning("Beware, vaccine delay is really important, are you sure you want to use this data ?")
  }

  dfVaccinsPreMerged <- dfVaccination_dep33 %>%
    filter(dep == "33") %>%
    mutate(START_DATE = DATE + delayVaccine) %>%
    select(START_DATE, Vaccin_1dose)

  ##### Weather
  delayWeather <- as.numeric(max(dfFullDashboard$START_DATE) - max(weather_data_dep33$DATE))

  if(delayWeather > 20){
    warning("Beware, weather delay is really important, are you sure you want to use this data ?")
  }

  weatherPreMerge <- weather_data_dep33 %>%
    mutate(START_DATE = DATE + delayWeather) %>%
    select(START_DATE, t.mean, precip, RH.mean, AH.mean, IPTCC.mean, ws.mean, dewpoint.mean)

  ##### MergeAll
  if(is.null(dfVariants)){
    dfDashboardRawTemp <- dfDashboardRaw
  } else {
    dfDashboardRawTemp <- dfDashboardRaw %>%
      left_join(dfVariantsPreMerged, by = "START_DATE") %>%
      mutate(Majority_variant = if_else(is.na(Majority_variant),
                                        "wild",
                                        Majority_variant)) # Variants
  }

  dfDashboardRawAll <- dfDashboardRawTemp %>%
    left_join(dfVaccinsPreMerged, by = "START_DATE") %>% # Variants
    mutate(across(.cols = any_of(colnames(dfVaccinsPreMerged %>% select(-START_DATE))),
                  .fns = ~if_else(START_DATE < min(dfVaccinsPreMerged$START_DATE), 0, as.numeric(.x)))) %>%
    left_join(weatherPreMerge, by = "START_DATE") %>% # Weather
    arrange(START_DATE) %>%
    tidyr::fill(any_of(colnames(weatherPreMerge))) # LOCF imputation for few february missing data (4 empty and 3 empty)

  # dfDashboard <- ComputerolMeanMaxMinDeriv(dfDashboardRaw)
  # dfDashboardAll <- ComputerolMeanMaxMinDeriv(dfDashboardRawAll,
  #                                             excludeTansf = vecExcludeTransf)
  #
  # return(list(dfDashboardRaw = dfDashboardRaw,
  #             dfDashboardRawAll = dfDashboardRawAll,
  #             dfDashboard = dfDashboard,
  #             dfDashboardAll = dfDashboardAll))

  return(dfDashboardRawAll)
}
