## load packages
library(dplyr)
library(PredictCovid)

## load data
dfEDS <- readRDS(file = "extdata/publication_datasets/lsEDSOpen.rds")$dfEDS

##### remove features not from EDS for copyright

vec_colnames <- dfEDS %>% colnames()

vec_keep <- c("START_DATE",
              "hosp",
              "WEEKDAY",
              grep(x = vec_colnames, pattern = "^SAMU_", value = TRUE),
              grep(x = vec_colnames, pattern = "^URG_", value = TRUE),
              grep(x = vec_colnames, pattern = "^PCR_INTRA_EXTRA_prelevements_", value = TRUE),
              grep(x = vec_colnames, pattern = "^IN_HOSP_", value = TRUE),
              grep(x = vec_colnames, pattern = "^OUT_HOSP_", value = TRUE),
              grep(x = vec_colnames, pattern = "^OUT_ICU_", value = TRUE),
              grep(x = vec_colnames, pattern = "^IN_ICU_", value = TRUE),
              grep(x = vec_colnames, pattern = "^P_", value = TRUE),
              grep(x = vec_colnames, pattern = "^TESTED_", value = TRUE),
              grep(x = vec_colnames, pattern = "^FRACP_", value = TRUE))

dfEDS <- dfEDS %>% select(all_of(vec_keep))

##### Obfuscation

## obfuscation function
obfuscate <- function(x) if_else(x <= 10, true = 0, false = x)

## features not to obfuscate
vec_noobfuscate_features <- c("START_DATE", "WEEKDAY")

## obfuscation
dfEDSobfuscated <- dfEDS %>%
  mutate(across(.cols = !all_of(vec_noobfuscate_features),
                .fns = obfuscate)) %>%
  mutate(FRACP_0_19_ANS = P_0_19_ANS/TESTED_0_19_ANS,
         FRACP_20_59_ANS = P_20_59_ANS/TESTED_20_59_ANS,
         FRACP_60_90_PLUS_ANS = P_60_90_PLUS_ANS/TESTED_60_90_PLUS_ANS,
         FRACP_TOUS_AGES = P_TOUS_AGES/TESTED_TOUS_AGES) %>%
  mutate(across(.cols = c(FRACP_0_19_ANS, FRACP_20_59_ANS, FRACP_60_90_PLUS_ANS, FRACP_TOUS_AGES),
                .fns = function(x) if_else(is.na(x), 0, x))) %>%
  janitor::remove_constant()

saveRDS(object = dfEDSobfuscated,
        file = "extdata/test_deploy/dfEDSobfuscated.rds")
