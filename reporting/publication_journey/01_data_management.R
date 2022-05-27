library(PredictCovid)
library(dplyr)

##### load data #####
lsEDSOpen <- readRDS(file = "extdata/publication_datasets/lsEDSOpen.rds")

dfEDS <- lsEDSOpen$dfEDS
dfOpen <- lsEDSOpen$lsOpenData$dfData

##### define hyperparameters #####

# 1) PCR + hospitalisation + Population + Weekday
# 2) 1 + weather + variants + vaccination
# 3) 2 + switch chu bordeaux only + data from Gironde dep
# 4) 3 + SAMU + Urgences + TDM
# 5) 4 + plusieurs modèles (rf, poisson, linear)
# 6) 5 + lissage
# 7) 6 + interaction vaccin + PCR_tous_ages

vecweather <- c("precip", "RH.mean", "AH.mean", "IPTCC.mean", "ws.mean", "dewpoint.mean", "t.mean")
vecGirondeHospPCR <- c("GIRONDE_HOSP",
                       "GRP_GIRONDE_P_0_19_ANS", "GRP_GIRONDE_P_20_59_ANS", "GRP_GIRONDE_P_60_90_PLUS_ANS", "GRP_GIRONDE_P_TOUS_AGES",
                       "GRP_GIRONDE_TESTED_0_19_ANS", "GRP_GIRONDE_TESTED_20_59_ANS", "GRP_GIRONDE_TESTED_60_90_PLUS_ANS", "GRP_GIRONDE_TESTED_TOUS_AGES",
                       "GRP_GIRONDE_FRACP_0_19_ANS", "GRP_GIRONDE_FRACP_20_59_ANS", "GRP_GIRONDE_FRACP_60_90_PLUS_ANS", "GRP_GIRONDE_FRACP_TOUS_AGES")

vecHospPCR <- c("START_DATE", "dep", "reg", "Population", "WEEKDAY", "hosp",
                "P_0_19_ANS", "P_20_59_ANS", "P_60_90_PLUS_ANS", "P_TOUS_AGES",
                "TESTED_0_19_ANS", "TESTED_20_59_ANS", "TESTED_60_90_PLUS_ANS", "TESTED_TOUS_AGES",
                "FRACP_0_19_ANS", "FRACP_20_59_ANS", "FRACP_60_90_PLUS_ANS", "FRACP_TOUS_AGES")
vecHospPCRWeather <- c(vecHospPCR, vecweather)
vecHospPCRWeatherVariantsVaccine <- colnames(dfOpen)
vecHospPCRGironde <- c(vecHospPCR[!vecHospPCR %in% c("dep", "reg", "Population")],
                       vecGirondeHospPCR)
vecHospPCRGirondeWeather <- c(vecHospPCRGironde, vecweather)
vecEDSGirondeWeather <- colnames(dfEDS)[!grepl(x = colnames(dfEDS), pattern = "^INTER|Vaccin_1dose|Majority_variant")]
vecEDSGirondeWeatherVariantsVaccine <- colnames(dfEDS)[!grepl(x = colnames(dfEDS), pattern = "^INTER")]
vecEDSGirondeWeatherVariantsVaccineInterVariant <- colnames(dfEDS)[!grepl(x = colnames(dfEDS), pattern = "^INTERVACCIN")]
vecEDSGirondeWeatherVariantsVaccineInterVaccine <- colnames(dfEDS)[!grepl(x = colnames(dfEDS), pattern = "^INTERVARIANT")]
vecHospPCRGirondeWeatherVariantVaccine <- c(vecHospPCRGirondeWeather,
                                            grep(x = colnames(dfEDS), pattern = "^Vaccin_1dose|Majority_variant",
                                                 value = TRUE))

lsFeatures <- list(HospPCR = vecHospPCR,
                   HospPCRWeather = vecHospPCRWeather,
                   HospPCRWeatherVariantsVaccine = vecHospPCRWeatherVariantsVaccine,
                   HospPCRGironde = vecHospPCRGironde,
                   HospPCRGirondeWeather = vecHospPCRGirondeWeather,
                   EDSGirondeWeather = vecEDSGirondeWeather,
                   EDSGirondeWeatherVariantsVaccine = vecEDSGirondeWeatherVariantsVaccine,
                   EDSGirondeWeatherVariantsVaccineInterVariant = vecEDSGirondeWeatherVariantsVaccineInterVariant,
                   EDSGirondeWeatherVariantsVaccineInterVaccine = vecEDSGirondeWeatherVariantsVaccineInterVaccine,
                   HospPCRGirondeWeatherVariantVaccine = vecHospPCRGirondeWeatherVariantVaccine)

# dfSteps <- list(expand.grid(step = c("step1", "step2", "step1bis"),
#                             span = c(0, 7, 14, 21),
#                             model = "enet",
#                             df = c("dfOpen", "dfEDS")),
#                 expand.grid(step = c("step3", "step5"),
#                             span = c(0, 7, 14, 21),
#                             model = "enet",
#                             df = c("dfEDS")),
#                 expand.grid(step = c("step4"),
#                             span = c(0, 7, 14, 21),
#                             model = c("enet", "poisson", "rf"),
#                             df = c("dfEDS"))) %>%
#   bind_rows() %>%
#   filter(span == 21)

## Date vector
vecDate <- dfOpen %>%
  filter(START_DATE >= as.Date("2020-12-01")) %>%
  pull(START_DATE) %>%
  unique()

dfSteps <- list(
  # dfOpen features
  expand.grid(step = c("HospPCR", "HospPCRWeather",
                       "HospPCRWeatherVariantsVaccine"),
              span = 21,
              model = "enet",
              rolderiv = TRUE,
              df = c("dfEDS", "dfOpen"),
              forecast = c(7, 14)),
  # dfEDS features
  expand.grid(step = c("HospPCRGironde", "HospPCRGirondeWeather",
                       "EDSGirondeWeather", "EDSGirondeWeatherVariantsVaccine"),
              span = 21,
              model = "enet",
              rolderiv = TRUE,
              df = "dfEDS",
              forecast = c(7, 14)),
  # day 1 to 14
  expand.grid(step = "EDSGirondeWeatherVariantsVaccine",
              span = 21,
              model = "enet",
              rolderiv = TRUE,
              df = "dfEDS",
              forecast = c(1:14)),
  # time forecast comparison
  expand.grid(step = c("EDSGirondeWeatherVariantsVaccine"),
              span = 21,
              model = "enet",
              rolderiv = TRUE,
              df = "dfEDS",
              forecast = c(7, 11, 14, 18)),
  # feature engineering comparison
  expand.grid(step = c("EDSGirondeWeatherVariantsVaccine"),
              span = 21,
              model = "enet",
              rolderiv = FALSE,
              df = "dfEDS",
              forecast = c(7, 14)),
  # smoothing comparison
  expand.grid(step = c("EDSGirondeWeatherVariantsVaccine"),
              span = c(0, 7, 14, 21),
              model = "enet",
              rolderiv = TRUE,
              df = "dfEDS",
              forecast = c(7, 14)),
  # model comparison
  expand.grid(step = c("EDSGirondeWeatherVariantsVaccine"),
              span = 21,
              model = c("enet", "poisson", "rf"),
              rolderiv = TRUE,
              df = "dfEDS",
              forecast = c(7, 14))) %>%
  bind_rows() %>%
  distinct() %>%
  merge(., data.frame(Date = vecDate)) %>%
  tibble::rowid_to_column(var = "STEP_ID")

### Frechet ###
dfFrechet <- dfSteps %>%
  filter(step == "EDSGirondeWeatherVariantsVaccine",
         span == 21,
         rolderiv == "FALSE",
         df == "dfEDS",
         model == "enet",
         forecast %in% c(7, 14)) %>%
  mutate(model = "frechet") %>%
  select(-STEP_ID) %>%
  tibble::rowid_to_column(var = "STEP_ID")

### Interaction ###
unitaryBest <- dfSteps %>%
  filter(step == "EDSGirondeWeatherVariantsVaccine",
         span == 21,
         rolderiv == "TRUE",
         df == "dfEDS",
         model == "enet",
         forecast %in% c(7, 14)) %>%
  select(-STEP_ID)

dfInteraction <- bind_rows(unitaryBest %>%
                             mutate(step = "EDSGirondeWeatherVariantsVaccineInterVariant"),
                           unitaryBest %>%
                             mutate(step = "EDSGirondeWeatherVariantsVaccineInterVaccine")) %>%
  bind_rows() %>%
  tibble::rowid_to_column(var = "STEP_ID")

### EDS / no EDS ###
dfwithEDS <- dfSteps %>%
  filter(span == 21,
         model == "enet",
         rolderiv == "TRUE",
         df == "dfEDS",
         forecast %in% c(7, 14),
         step == "EDSGirondeWeatherVariantsVaccine")
dfwithoutEDS <- dfwithEDS %>%
  mutate(step = "HospPCRGirondeWeatherVariantVaccine")

dfstepsEDSnoEDS <- bind_rows(dfwithEDS, dfwithoutEDS) %>%
  select(-STEP_ID) %>%
  tibble::rowid_to_column(var = "STEP_ID")

#### save hyperparameters ####
saveRDS(object = list(dfSteps = dfSteps,
                      lsFeatures = lsFeatures,
                      dfFrechet = dfFrechet,
                      dfInteraction = dfInteraction,
                      dfstepsEDSnoEDS = dfstepsEDSnoEDS),
        file = "extdata/publication_datasets/lsHyperparamPublication.rds")
