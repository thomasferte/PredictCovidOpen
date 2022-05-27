#' ImportGompertzImputeVariants
#'
#' @description Import Variants df and impute missing data with Gompertz law
#'
#' @param fileVariantsBefore062021 the url of the variants before 06 2021.
#' @param fileVariantsAfter062021 the url of the variants after 06 2021.
#' @param datePlateauVariants the max date of the data used to train Gompertz. This is necessary because of the decrease in april.
#' @param dfVariantsImputed The previous dataset with imputation. If not provided, the Gompertz imputation is performed for missing values.
#'
#' @return A dataframe with department, date, variants percentage
#' @importFrom purrr map
#' @export
ImportGompertzImputeVariants <- function(fileVariantsBefore062021 = "https://www.data.gouv.fr/en/datasets/r/16f4fd03-797f-4616-bca9-78ff212d06e8",
                                         fileVariantsAfter062021 = "https://www.data.gouv.fr/fr/datasets/r/4d3e5a8b-9649-4c41-86ec-5420eb6b530c",
                                         datePlateauVariants = as.Date("2021-04-15"),
                                         dfVariantsImputed = NULL){

  # import data
  lsVariantsRaw <- LoadVariants(fileVariantsBefore062021 = fileVariantsBefore062021,
                                fileVariantsAfter062021 = fileVariantsAfter062021)

  if(is.null(dfVariantsImputed)){
    ## impute missing variants data with Gompertz law on data until plateau
    dfres <- ImputeVariantsGompertz(dfVariantsRaw = lsVariantsRaw$dfVariantsRawBefore062021,
                                    datePlateauVariants = datePlateauVariants)
  } else {
    dfres <- dfVariantsImputed %>%
      full_join(lsVariantsRaw$dfVariantsRawBefore062021 %>%
                  rename("VARIANTS_RAW" = "Prc_susp_501Y_V1_V2_V3_IND")) %>%
      mutate(Prc_susp_501Y_V1_V2_V3_IND = if_else(is.na(VARIANTS_RAW),
                                                  Prc_susp_501Y_V1_V2_V3_IND,
                                                  VARIANTS_RAW)) %>%
      select(DATA_DAY, dep, Prc_susp_501Y_V1_V2_V3_IND)
  }

  return(dfres)
}
