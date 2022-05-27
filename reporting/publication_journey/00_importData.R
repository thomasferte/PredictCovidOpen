##### load packages #####
library(PredictCovid)
library(dplyr)

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
dfVariants <- ImportMajorityVariant()

### Import vaccination data
dfVaccination <- ImportCleanVaccination() %>%
  select(dep, jour, n_cum_dose1_tous_ages) %>%
  rename("DATE" = "jour",
         "Vaccin_1dose" = "n_cum_dose1_tous_ages")

### Import 2022 weather (2020-2021 already stored)
weather_data_byDep2022 <- weather_data_from_NOAA(Regions_or_Dept_stations_pop = Dept_stations_pop,
                                                 years = 2022,
                                                 n.cores = 1) %>%
  select(date_day, code_insee, t.mean,
         precip, RH.mean, AH.mean,
         IPTCC.mean, ws.mean, dewpoint.mean) %>%
  rename("DATE" = "date_day",
         "dep" = "code_insee")

## merge
lsWeather <- bind_rows(weather_data_byDep2022,
                       weather_data_byDepHistorical) %>%
  ImputeMissingWeather()

weather_data_byDep <- lsWeather$dfImputed

### Import PCR and hospitalisation data
df_hospitPCR <- datagouv_importHospitPCR() %>%
  mutate(dep = gsub(pattern = "_SIDEP", replacement = "", x = dep))

###############################################
######### Merge all ###########################
###############################################

### EDS data
dfFullDashboard <- CovidDataFromI2b2(RqCOVID_INDIC = RqCOVID_INDIC,
                                     RqCOVID_FEATURES_SAVE_BIS = RqCOVID_FEATURES_SAVE_BIS)


dfEDS <- MergeDashboardVaccinVariantsWeather(dfFullDashboard = dfFullDashboard,
                                             dfVariants_dep33 = dfVariants %>% filter(dep == "33"),
                                             dfVaccination_dep33 = dfVaccination %>% filter(dep == "33"),
                                             weather_data_dep33 = weather_data_byDep %>% filter(dep == "33")) %>%
  CleanEDS(dfEDS = .)

### open data
lsOpenData <- MergeOpenData(dfVariants = dfVariants,
                            dfVaccination = dfVaccination,
                            weather_data_byDep = weather_data_byDep,
                            df_hospitPCR = df_hospitPCR)

lsOpenData$dfData <- CleanOpen(dfEDS = dfEDS,
                               dfOpen = lsOpenData$dfData)

###############################################
######### Save all ############################
###############################################
res <- list(dfEDS = dfEDS,
            lsOpenData = lsOpenData,
            lsWeatherDiag = lsWeather$Diag)

saveRDS(res, file = "extdata/publication_datasets/lsEDSOpen.rds")
