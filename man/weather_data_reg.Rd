% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weather_data_reg.R
\docType{data}
\name{weather_data_reg}
\alias{weather_data_reg}
\title{Weather dataset per French region (2021-03-08)}
\format{
A data frame with 5024 rows and 10 variables
}
\usage{
weather_data_reg
}
\description{
A dataset containing the weather parameters according to
National Oceanic and Atmospheric Administration as of 2021-03-08.
The variables are as follows:
}
\details{
\itemize{
  \item label_insee region name
  \item code_insee insee code, 1 per region
  \item date_day Date of meteorological data
  \item stat_t.mean Mean temperature over the day
  \item stat_precip Precipitation over the day
  \item stat_RH.mean Mean relative humidity over the day
  \item stat_AH.mean Mean absolute humidity over the day
  \item stat_IPTCC.mean mean IPTCC index over the day
  \item stat_ws.mean mean wind speed over the day
  \item stat_dewpoint.mean Mean dew point ("point de rosée") over the day.
}
}
\examples{
##### script generating meteo dataframe
\dontrun{
weather_data_reg <- weather_data_from_NOAA(
     Regions_or_Dept_stations_pop = SEIRcovid19FR::Regions_stations_pop,
     years = 2020:2021,
     n.cores = 2
     )
save(weather_data_reg, file = "weather_data_reg.rdata")
}
}
\keyword{datasets}
