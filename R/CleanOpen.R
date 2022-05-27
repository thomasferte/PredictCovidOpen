#' CleanOpen
#'
#' @description Clean open data dataframe and EDS dataframe.
#'
#' @param dfEDS The data from EDS
#' @param dfOpen The data from Open data
#' @param delayEDSOpen The delay between both sources
#'
#' @return A list of length two with both cleaned dataframe
#' @export
CleanOpen <- function(dfEDS,
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
           Population = popGironde/2)

  dfOpenDepEDS <- dfOpenDepLevel %>%
    bind_rows(dfEDSDepLevel %>% select(all_of(colnames(dfOpenDepLevel)))) %>%
    mutate(reg = as.factor(reg))

  return(dfOpen = dfOpenDepEDS)
}

