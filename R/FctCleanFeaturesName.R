#' FctCleanFeaturesName
#'
#' @description Clean features names (more explicit names, convert French to English)
#'
#' @param vecFeatures A character vector of features names
#'
#' @return A character vector with the cleaned features names.
#' @export
#' @importFrom stringr str_sub
FctCleanFeaturesName <- function(vecFeatures){
  result <- gsub(x = vecFeatures, pattern = "hosp", replacement = "Hospit") %>%
    # METEO
    gsub(pattern = "stat_t.mean_", replacement = "Temperature ") %>%
    gsub(pattern = "stat_precip_", replacement = "Precipitation ") %>%
    gsub(pattern = "stat_RH.mean_", replacement = "Relative humidity ") %>%
    gsub(pattern = "stat_AH.mean_", replacement = "Absolute humidity ") %>%
    gsub(pattern = "stat_IPTCC.mean_", replacement = "IPTCC ") %>%
    gsub(pattern = "stat_ws.mean_", replacement = "Wind speed ") %>%
    gsub(pattern = "stat_dewpoint.mean_", replacement = "Dew point ") %>%
    # hosp and test
    gsub(pattern = "fct_firstDeriv.*_fct_firstDeriv", replacement = " 2nd variation ") %>%
    gsub(pattern = "_fct_firstDeriv", replacement = " variation ") %>%
    gsub(pattern = "_fct_meanCases", replacement = " avg ") %>%
    gsub(pattern = "P_", replacement = "PCR+ ") %>%
    gsub(pattern = "TESTED_", replacement = "PCR tests ") %>%
    gsub(pattern = "Frac", replacement = "Proportion ") %>%
    gsub(pattern = "_ans", replacement = " yo") %>%
    gsub(pattern = "tous_ages", replacement = "") %>%
    gsub(pattern = "_et_plus", replacement = "") %>%
    # regions
    gsub(pattern = "reg_11", replacement = "Ile-de-France") %>%
    gsub(pattern = "reg_24", replacement = "Centre-Val de Loire") %>%
    gsub(pattern = "reg_27", replacement = "Bourgogne-Franche-Comte") %>%
    gsub(pattern = "reg_28", replacement = "Normandie") %>%
    gsub(pattern = "reg_32", replacement = "Hauts-de-France") %>%
    gsub(pattern = "reg_44", replacement = "Grand Est") %>%
    gsub(pattern = "reg_52", replacement = "Pays de la Loire") %>%
    gsub(pattern = "reg_53", replacement = "Bretagne") %>%
    gsub(pattern = "reg_75", replacement = "Nouvelle-Aquitaine") %>%
    gsub(pattern = "reg_76", replacement = "Occitanie") %>%
    gsub(pattern = "reg_84", replacement = "Auvergne-Rhone-Alpes") %>%
    gsub(pattern = "reg_93", replacement = "Provence-Alpes-Cote d'Azur") %>%
    gsub(pattern = "reg_94", replacement = "Corse") %>%
    gsub(pattern = "reg_1", replacement = "Guadeloupe") %>%
    gsub(pattern = "reg_2", replacement = "Martinique") %>%
    gsub(pattern = "reg_3", replacement = "Guyane") %>%
    gsub(pattern = "reg_4", replacement = "La Reunion") %>%
    gsub(pattern = "reg_6", replacement = "Mayotte") %>%
    #
    gsub(pattern = "90", replacement = "90+") %>%
    gsub(pattern = "_", replacement = "-") %>%
    gsub(pattern = "dep", replacement = "Departement") %>%
    gsub(pattern = "reg", replacement = "Region") %>%
    gsub(pattern = "TIME", replacement = "Time") %>%
    gsub(pattern = "WEEKDAY", replacement = "Weekday") %>%
    gsub(pattern = "  ", replacement = " ") %>%
    if_else(condition = stringr::str_sub(string = ., -1) %in% c(0:9), true = paste0(., "d"), false = .) %>%
    trimws()
  return(result)
}
