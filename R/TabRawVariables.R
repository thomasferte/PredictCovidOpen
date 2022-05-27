#' TabRawVariables
#'
#' @description Generate table of variables available in dfDashboard
#'
#' @param dfDashboard The dataframe of interest.
#'
#' @return A table of features
#' @export
TabRawVariables <- function(dfDashboard){
  vecColnamesDashboard <- dfDashboard %>%
    select(-START_DATE) %>%
    colnames() %>%
    gsub(pattern = "_PERCENT|_COUNT|_rolMean|_rolMin|_rolMax|_rolDeriv.*", replacement = "") %>%
    unique() %>%
    sort()

  tabRawVariables <- data.frame(Features = vecColnamesDashboard) %>%
    mutate(Source = NA_character_,
           Source = if_else(Features %in% c("AH.mean", "dewpoint.mean", "IPTCC.mean", "precip", "RH.mean", "t.mean", "ws.mean"), "Weather", Source),
           Source = if_else(Features %in% c("Pct_Variants"), "Variants", Source),
           Source = if_else(Features %in% c("CHU_HOSP", "IN_HOSP_in", "IN_ICU_in", "OUT_HOSP_out", "OUT_ICU_out"), "CHU Hosp", Source),
           Source = if_else(Features %in% c(grep(pattern = "GRP_CHU_|PCR_INTRA_EXTRA_prelevements", x = Features, value = T)), "CHU PCR", Source),
           Source = if_else(Features %in% c(grep(pattern = "n_cum_dose", x = Features, value = T)), "Vaccine", Source),
           Source = if_else(Features %in% c(grep(pattern = "SAMU_", x = Features, value = T)), "SAMU", Source),
           Source = if_else(Features %in% c(grep(pattern = "URG_", x = Features, value = T)), "Emergency", Source),
           Source = if_else(Features %in% c(grep(pattern = "TDM_", x = Features, value = T)), "CT-scan", Source),
           Source = if_else(Features %in% c("CHU_WEEKDAY"), "Other", Source),
           Source = if_else(Features %in% c("GIRONDE_HOSP", grep(pattern = "GRP_GIRONDE_", x = Features, value = T)), "SIDEP", Source),
           Source = if_else(Features %in% c("GIRONDE_HOSP"), "SIDEP", Source)) %>%
    arrange(Source, Features)

  return(tabRawVariables)
}
