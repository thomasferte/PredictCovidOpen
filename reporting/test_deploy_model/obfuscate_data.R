## load packages
library(dplyr)
library(PredictCovid)

## load data
dfEDS <- readRDS(file = "extdata/publication_datasets/lsEDSOpen.rds")$dfEDS

## obfuscation function
obfuscate <- function(x) if_else(x <= 10, true = 0, false = x)

## features not to obfuscate
vec_noobfuscate_features <- c("START_DATE", "WEEKDAY", "GIRONDE_HOSP",
                              "Majority_variant", "Vaccin_1dose",
                              "t.mean", "precip", "RH.mean", "AH.mean", "IPTCC.mean", "ws.mean", "dewpoint.mean",
                              dfEDS %>% colnames() %>% grep(pattern = "^GRP_GIRONDE", value = TRUE))

## features to recompute after obfuscation
vec_features_compute_after_obf <- c(dfEDS %>% colnames() %>% grep(pattern = "^INTER", value = TRUE),
                                    dfEDS %>% colnames() %>% grep(pattern = "^FRACP", value = TRUE))

## obfuscation
dfEDSDepLevel <- dfEDS %>%
  select(- all_of(vec_features_compute_after_obf)) %>%
  mutate(across(.cols = !all_of(vec_noobfuscate_features),
                .fns = obfuscate)) %>%
  mutate(FRACP_0_19_ANS = P_0_19_ANS/TESTED_0_19_ANS,
         FRACP_20_59_ANS = P_20_59_ANS/TESTED_20_59_ANS,
         FRACP_60_90_PLUS_ANS = P_60_90_PLUS_ANS/TESTED_60_90_PLUS_ANS,
         FRACP_TOUS_AGES = P_TOUS_AGES/TESTED_TOUS_AGES) %>%
  mutate(across(.cols = c(FRACP_0_19_ANS, FRACP_20_59_ANS, FRACP_60_90_PLUS_ANS, FRACP_TOUS_AGES),
                .fns = function(x) if_else(is.na(x), 0, x)))

## add interaction between all PCR features and all variants forms
vecPCRfeatures <- grep(colnames(dfEDSDepLevel),
                       pattern = "^FRAC|^TESTED|^P_|^GRP_GIRONDE",
                       value = TRUE)
vecVariants <- dfEDSDepLevel$Majority_variant %>% unique()
varVaccin <- "Vaccin_1dose"
grid_interaction <- expand.grid(variant = vecVariants, pcr = vecPCRfeatures)

# create dfEDSInteraction with all interaction features with variant
dfEDSInteraction <- dfEDSDepLevel
apply(grid_interaction,
      1,
      function(row){
        pcr <- as.character(row[["pcr"]])
        variant <- as.character(row[["variant"]])

        varname <- paste("INTERVARIANT", variant, pcr, sep = "_")

        binary_variant <- as.numeric(dfEDSInteraction$Majority_variant == variant)

        dfEDSInteraction[[varname]] <<- binary_variant*dfEDSInteraction[[pcr]]

        return()
      })
# create dfEDSInteraction with all interaction features with vaccin
lapply(vecPCRfeatures,
       function(pcr){
         varname <- paste("INTERVACCIN", pcr, sep = "_")

         dfEDSInteraction[[varname]] <<- dfEDSInteraction[[varVaccin]]/10^6*dfEDSInteraction[[pcr]]

         return()
       })

dfEDSobfuscated <- dfEDSInteraction %>%
  janitor::remove_constant()

saveRDS(object = dfEDSobfuscated,
        file = "extdata/test_deploy/dfEDSobfuscated.rds")
