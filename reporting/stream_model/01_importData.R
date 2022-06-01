###############################################
######### import data from i2b2 ###############
###############################################

##### get connection #####
conn <- i2b2bordeaux::getI2B2con()

## dashboard
qGetDashboard <- "SELECT *
  FROM IAM.COVID_INDIC
  WHERE SAVE_DATE = (
    SELECT max(save_date)
    FROM IAM.COVID_INDIC
  )
  AND DOMAIN != 'PREL'
  AND DOMAIN != 'POS_PREL_BY_AGE'
  AND DOMAIN != 'TDM'"
RqCOVID_INDIC <- i2b2bordeaux::oracleDBquery(conn = conn,
                                             statement = qGetDashboard)

# get the max date
qFeatures <- "SELECT *
  FROM IAM.COVID_FEATURES_SAVE_BIS
  WHERE SAVE_DATE = (
    SELECT max(save_date)
    FROM IAM.COVID_FEATURES_SAVE_BIS
  )"
RqCOVID_FEATURES_SAVE_BIS <- i2b2bordeaux::oracleDBquery(conn = conn,
                                                         statement = qFeatures)

###############################################
######### import data from open ###############
###############################################

### Import majority variant
dfVariants <- PredictCovid::ImportMajorityVariant() %>%
  filter(dep == "33")

### Import vaccination data
dfVaccination <- PredictCovid::ImportCleanVaccination() %>%
  dplyr::select(dep, jour, n_cum_dose1_tous_ages) %>%
  dplyr::rename("DATE" = "jour",
                "Vaccin_1dose" = "n_cum_dose1_tous_ages") %>%
  dplyr::filter(dep == "33")

### Import 2022 weather data (2020-2021 already stored)
weatherDepToImport <- c("33",
                        dfLimitrophe %>%
                          dplyr::filter(departement == "33") %>%
                          dplyr::pull(adjacent))
weather_data_byDep2022 <- PredictCovid::weather_data_from_NOAA(Regions_or_Dept_stations_pop = Dept_stations_pop[Dept_stations_pop$code_insee %in% weatherDepToImport,],
                                                               years = 2022,
                                                               n.cores = 1) %>%
  dplyr::select(date_day, code_insee, t.mean,
                precip, RH.mean, AH.mean,
                IPTCC.mean, ws.mean, dewpoint.mean) %>%
  dplyr::rename("DATE" = "date_day",
                "dep" = "code_insee")

## merge
lsWeather <- dplyr::bind_rows(weather_data_byDep2022,
                              weather_data_byDepHistorical) %>%
  PredictCovid::ImputeMissingWeather()

weather_data_byDep <- lsWeather$dfImputed %>%
  dplyr::filter(dep == "33")

###############################################
######### Merge all ###########################
###############################################

### EDS data
dfFullDashboard <- PredictCovid::CovidDataFromI2b2(RqCOVID_INDIC = RqCOVID_INDIC,
                                                   RqCOVID_FEATURES_SAVE_BIS = RqCOVID_FEATURES_SAVE_BIS)


dfEDS <- PredictCovid::MergeDashboardVaccinVariantsWeather(dfFullDashboard = dfFullDashboard,
                                                           dfVariants_dep33 = dfVariants,
                                                           dfVaccination_dep33 = dfVaccination,
                                                           weather_data_dep33 = weather_data_byDep) %>%
  PredictCovid::CleanEDS(dfEDS = .)
