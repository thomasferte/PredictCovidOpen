#' Update2021Weather
#'
#' @description Update 2021 weather data and add it to 2020 weather data.
#'
#' @param n.cores Number of cores
#'
#' @return A dataframe with 2020 and 2021 data
#' @export
Update2021Weather <- function(n.cores = 2){
  dfweather_data_dep2021 <- weather_data_from_NOAA(Regions_or_Dept_stations_pop = Dept_stations_pop,
                                                   years = 2021,
                                                   n.cores = n.cores)
  dfres <- bind_rows(dfweather_data_dep2020, dfweather_data_dep2021)
  return(dfres)

}
