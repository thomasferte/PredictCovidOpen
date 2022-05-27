#' Meteorological stations dataset per French departement (2021-03-03)
#'
#'A dataset containing the stations parameters at the departement level.
#'The variables are as follows:
#'
#' \itemize{
#'   \item code_insee departement insee code
#'   \item nom region name
#'   \item nuts3 Nomenclature of Territorial Units
#'   \item wikipedia wikipedia id
#'   \item surf_km2 surface covered in km2
#'   \item fid Geographical unique id
#'   \item usaf United States Air Force Id
#'   \item wban Wireless Body Area Network
#'   \item station Station name
#'   \item ctry country id
#'   \item st state id
#'   \item call
#'   \item latitude Latitude of the station
#'   \item longitude Longitude of the station
#'   \item elev.m. Altitude of the station
#'   \item begin Date of beginning of activity
#'   \item end Date of end of activity
#'   \item code Station code
#'   \item fra_pop_su Metropolitan French population on the station surface
#'   \item glp_pop_su Guadeloup population on the station surface
#'   \item guf_pop_su Guyanne population on the station surface
#'   \item mtq_pop_su Martinique population on the station surface
#'   \item myt_pop_su Mayotte population on the station surface
#'   \item reu_pop_su Reunion population surface
#' }
#' @format A data frame with 328 rows and 24 variables
#'
#'@examples
#'##### script generating meteo dataframe
#' \dontrun{
#' Dept_stations_pop <- SEIRcovid19FR::Dept_stations_pop
#' save(Dept_stations_pop, file = "data/Dept_stations_pop.rdata")
#' }
"Dept_stations_pop"

