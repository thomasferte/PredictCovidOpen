#' ImportOpenData
#'
#' @description Import data from covid openscience sources (vaccination, variants, weather, hospitalisation, pcr by age)
#'
#' @return A list of length two with the merged dataframe and the delay between datasources
#' @export
ImportOpenData <- function(){

  ### Import majority variant
  dfVariants <- ImportMajorityVariant() %>%
    mutate(Majority_variant = as.character(Majority_variant)) %>%
    rename("DATE" = "DATA_DAY")

  ### Import vaccination data
  dfVaccination <- ImportCleanVaccination() %>%
    select(dep, jour, n_cum_dose1_tous_ages) %>%
    rename("DATE" = "jour",
           "Vaccin_1dose" = "n_cum_dose1_tous_ages")

  ### Import weather data
  weather_data_byDep <- weather_data_from_NOAA(Regions_or_Dept_stations_pop = Dept_stations_pop,
                                               years = 2020:2021,
                                               n.cores = 1) %>%
    select(date_day, code_insee, t.mean,
           precip, RH.mean, AH.mean,
           IPTCC.mean, ws.mean, dewpoint.mean) %>%
    rename("DATE" = "date_day",
           "dep" = "code_insee")

  ### Import PCR and hospitalisation data
  df_hospitPCR <- PredictCovid::datagouv_importHospitPCR() %>%
    mutate(dep = gsub(pattern = "_SIDEP", replacement = "", x = dep))

  ### compute delay
  lsDelay <- lapply(X = list(Variants = dfVariants,
                             Vaccination = dfVaccination,
                             Weather = weather_data_byDep),
                    FUN = function(x){
                      delay <- max(df_hospitPCR$DATE) - max(x$DATE)

                      dfRes <- x %>% mutate(DATE = DATE + delay)

                      return(list(dfDelay = dfRes, delay = delay))
                    })

  ### Merge data
  dfMergedSidepData <- lapply(lsDelay,
                              function(x) x$dfDelay) %>%
    purrr::reduce(.f = dplyr::full_join, by = c("DATE", "dep")) %>%
    dplyr::right_join(df_hospitPCR, by = c("DATE", "dep"))

  ### Deal with NA
  lsMerge <- dfMergedSidepData %>%
    arrange(dep, DATE) %>%
    mutate(Vaccin_1dose = if_else(is.na(Vaccin_1dose) & DATE < as.Date("2021-01-01"),
                                  true = as.integer(0), false = Vaccin_1dose),
           Majority_variant = if_else(is.na(Majority_variant) & DATE < as.Date("2021-03-01"),
                                      true = "wild", false = Majority_variant)) %>%
    ImputeMissingWeather()

  lsDelaySources <- lapply(lsDelay, FUN = function(x) x$delay)

  return(list(dfData = lsMerge$dfImputed,
              delay = lsDelaySources,
              lsDiagImputation = lsMerge$Diag))
}
