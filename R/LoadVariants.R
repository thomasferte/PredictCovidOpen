#' LoadVariants
#'
#' @description Import Variants df
#'
#' @param fileVariantsBefore062021 the url of the variants before 06 2021.
#' @param fileVariantsAfter062021 the url of the variants after 06 2021.
#' @param fileVariantsAfter012022 the url of the variants after 01 2022.
#' @param addVariantsBefore062021 should the variants before 2021-06 be added. Deprecated.
#'
#' @return A dataframe with department, date, variants percentage
#' @export
LoadVariants <- function(fileVariantsBefore062021,
                         fileVariantsAfter062021,
                         fileVariantsAfter012022,
                         addVariantsBefore062021 = TRUE){

  dfVariantsRawAfter012022 <- read.csv(file = fileVariantsAfter012022, sep = ";") %>%
    mutate(DATA_DAY = substr(semaine, 12, 21) %>% as.Date("%Y-%m-%d")) %>% # clean age class and first day of week
    filter(!dep %in% c(975, 977, 978, "<NA>")) %>% # only 33 department and all age class
    select(DATA_DAY, dep, tx_A1, tx_C1, tx_D1)

  dfVariantsRawAfter062021 <- read.csv(file = fileVariantsAfter062021, sep = ";") %>%
    mutate(DATA_DAY = substr(semaine, 12, 21) %>% as.Date("%Y-%m-%d")) %>% # clean age class and first day of week
    filter(!dep %in% c(975, 977, 978),
           DATA_DAY < min(dfVariantsRawAfter012022$DATA_DAY)) %>% # only 33 department and all age class
    select(DATA_DAY, dep, tx_A1, tx_B1, tx_C1)

  dfVariantsRawBefore062021 <- read.csv(file = fileVariantsBefore062021, sep = ";") %>%
    mutate(cl_age90 = factor(cl_age90,
                             levels = dfClassesAgeVariants$indicateur_classe,
                             labels = dfClassesAgeVariants$Classe_age),
           DATA_DAY = substr(semaine, 12, 21) %>% as.Date("%Y-%m-%d")) %>% # clean age class and first day of week
    filter(cl_age90 == "tous \u00E2ges",
           !dep %in% c(975, 977, 978),
           DATA_DAY < min(dfVariantsRawAfter062021$DATA_DAY)) %>% # only 33 department and all age class
    select(DATA_DAY, dep, Prc_susp_501Y_V1, Prc_susp_501Y_V2_3, Prc_susp_IND)

  if(addVariantsBefore062021){
    dfVariantsRawBefore062021 <- dfVariantsRawBefore062021 %>%
      mutate(Prc_susp_501Y_V1_V2_V3_IND = Prc_susp_501Y_V1 + Prc_susp_501Y_V2_3 + Prc_susp_IND) %>% # group all variants into 1 feature
      select(DATA_DAY, dep, Prc_susp_501Y_V1_V2_V3_IND)
  }

  return(list(dfVariantsRawBefore062021 = dfVariantsRawBefore062021,
              dfVariantsRawAfter062021 = dfVariantsRawAfter062021,
              dfVariantsRawAfter012022 = dfVariantsRawAfter012022))
}
