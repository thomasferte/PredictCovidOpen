#' MergeOpenData
#'
#' @param dfVariants Variant data frame
#' @param dfVaccination Vaccine data frame
#' @param weather_data_byDep Weather data frame
#' @param df_hospitPCR Hospitalisation and PCR data frame
#'
#' @description Merge open data (vaccination, variants, weather, hospitalisation, pcr by age)
#'
#' @return A list of length two with the merged dataframe and the delay between datasources
#' @export
MergeOpenData <- function(dfVariants,
                          dfVaccination,
                          weather_data_byDep,
                          df_hospitPCR){

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
  lsMerge <- lapply(lsDelay,
                    function(x) x$dfDelay) %>%
    purrr::reduce(.f = dplyr::full_join, by = c("DATE", "dep")) %>%
    dplyr::right_join(df_hospitPCR, by = c("DATE", "dep")) %>%
    # deal with NA
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
