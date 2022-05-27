#' Dataframe with 2020 and 2021 weather data
#'
#'
#'@examples
#' ##### script generating meteo dataframe
#' \dontrun{
#' weather_data_byDepHistorical <- lapply(c(2020, 2021),
#'                                          function(year){
#'                                            print(year)
#'                                            weather_data_from_NOAA(Regions_or_Dept_stations_pop = Dept_stations_pop,
#'                                                                   years = year,
#'                                                                   n.cores = 1) %>%
#'                                              select(date_day, code_insee, t.mean,
#'                                                     precip, RH.mean, AH.mean,
#'                                                     IPTCC.mean, ws.mean, dewpoint.mean) %>%
#'                                              rename("DATE" = "date_day",
#'                                                     "dep" = "code_insee")
#'                                          }) %>%
#'     bind_rows()
#'   save(weather_data_byDepHistorical, file = "data/weather_data_byDepHistorical.rdata")
#' }
"weather_data_byDepHistorical"
