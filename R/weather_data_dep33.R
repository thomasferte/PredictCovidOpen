#' Weather dataset for Gironde
#'
#'A dataset containing the weather parameters according to
#'National Oceanic and Atmospheric Administration as of 2021-03-08.
#'The variables are as follows:
#'
#' \itemize{
#'   \item label_insee region name
#'   \item code_insee insee code, 1 per region
#'   \item date_day Date of meteorological data
#'   \item stat_t.mean Mean temperature over the day
#'   \item stat_precip Precipitation over the day
#'   \item stat_RH.mean Mean relative humidity over the day
#'   \item stat_AH.mean Mean absolute humidity over the day
#'   \item stat_IPTCC.mean mean IPTCC index over the day
#'   \item stat_ws.mean mean wind speed over the day
#'   \item stat_dewpoint.mean Mean dew point ("point de rosée") over the day.
#' }
#' @format A data frame with 5024 rows and 10 variables
#'
#'@examples
#' ##### script generating meteo dataframe
#' \dontrun{
#' weather_data_dep33 <- weather_data_from_NOAA(Regions_or_Dept_stations_pop = Dept_stations_pop[Dept_stations_pop$code_insee == 33,],
#' years = 2020:2021,
#' n.cores = 1)
#' saveRDS(object = weather_data_dep33, file = "data/weather_data_dep33.rds")
#'
#' }
"weather_data_dep33"
