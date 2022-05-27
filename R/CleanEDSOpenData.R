#' CleanEDSOpenData
#'
#' @description Clean open data dataframe and EDS dataframe.
#'
#' @param dfEDS The data from EDS
#' @param dfOpen The data from Open data
#' @param delayEDSOpen The delay between both sources
#'
#' @return A list of length two with both cleaned dataframe
#' @export
CleanEDSOpenData <- function(dfEDS,
                             dfOpen,
                             delayEDSOpen = 4){
  popGironde <- dfOpen %>%
    filter(dep == "33") %>%
    pull(Population) %>%
    unique()

  maxDate <- min(max(dfEDS$START_DATE), max(dfOpen$DATE + delayEDSOpen))
  minDate <- max(min(dfEDS$START_DATE), min(dfOpen$DATE + delayEDSOpen))

  ##### dep level data #####
  dfOpenDepLevel <- dfOpen %>%
    ungroup() %>%
    mutate(DATE = DATE + delayEDSOpen,
           P_0_19_ANS = P_0_9_ans + P_10_19_ans,
           P_20_59_ANS = P_20_29_ans + P_30_39_ans + P_40_49_ans + P_50_59_ans,
           P_60_90_PLUS_ANS = P_60_69_ans + P_70_79_ans + P_80_89_ans + P_90_ans_et_plus,
           TESTED_0_19_ANS = TESTED_0_9_ans + TESTED_10_19_ans,
           TESTED_20_59_ANS = TESTED_20_29_ans + TESTED_30_39_ans + TESTED_40_49_ans + TESTED_50_59_ans,
           TESTED_60_90_PLUS_ANS = TESTED_60_69_ans + TESTED_70_79_ans + TESTED_80_89_ans + TESTED_90_ans_et_plus,
           FRACP_0_19_ANS = if_else(TESTED_0_19_ANS == 0, 0, P_0_19_ANS/TESTED_0_19_ANS),
           FRACP_20_59_ANS = if_else(TESTED_20_59_ANS == 0, 0, P_20_59_ANS/TESTED_20_59_ANS),
           FRACP_60_90_PLUS_ANS = if_else(TESTED_60_90_PLUS_ANS == 0, 0, P_60_90_PLUS_ANS/TESTED_60_90_PLUS_ANS)) %>%
    rename_at(.vars = c("P_tous_ages", "TESTED_tous_ages", "FracP_tous_ages"),
              .funs = toupper) %>%
    rename("START_DATE" = "DATE") %>%
    select(START_DATE, dep, reg,
           Population, WEEKDAY,
           Majority_variant,
           Vaccin_1dose,
           precip, RH.mean, AH.mean, IPTCC.mean, ws.mean, dewpoint.mean,
           hosp,
           P_0_19_ANS, P_20_59_ANS, P_60_90_PLUS_ANS, P_TOUS_AGES,
           TESTED_0_19_ANS, TESTED_20_59_ANS, TESTED_60_90_PLUS_ANS, TESTED_TOUS_AGES,
           FRACP_0_19_ANS, FRACP_20_59_ANS, FRACP_60_90_PLUS_ANS, FRACP_TOUS_AGES) %>%
    filter(START_DATE <= maxDate & START_DATE >= minDate)

  dfEDSDepLevel <- dfEDS %>%
    mutate(dep = "CHU_BDX",
           reg = 75,
           Population = popGironde/2) %>%
    rename("Vaccin_1dose" = "n_cum_dose1_tous_ages",
           "hosp" = "CHU_HOSP",
           "WEEKDAY" = "CHU_WEEKDAY") %>%
    left_join(dfOpenDepLevel %>%
                filter(dep == "33") %>%
                select(START_DATE, Majority_variant),
              by = "START_DATE") %>%
    rename_with(.cols = starts_with("GRP_CHU_"),
                .fn = function(x) gsub(pattern = "GRP_CHU_", replacement = "", x = x)) %>%
    filter(START_DATE <= maxDate & START_DATE >= minDate) %>%
    select(-c(n_cum_dose1_18_59, n_cum_dose1_60_80_PLUS))

  dfOpenDepEDS <- dfOpenDepLevel %>%
    bind_rows(dfEDSDepLevel %>% select(all_of(colnames(dfOpenDepLevel)))) %>%
    mutate(reg = as.factor(reg))


  ## add intereaction between all PCR features and all variants forms
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

  dfEDSSpecific <- dfEDSInteraction%>%
    janitor::remove_constant()

  return(list(dfEDS = dfEDSSpecific,
              dfOpen = dfOpenDepEDS))
}

