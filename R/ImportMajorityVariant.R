#' ImportMajorityVariant
#'
#' @description Import Variants df and find the majority variant at a given date by department.
#'
#' @param fileVariantsBefore062021 the url of the variants before 06 2021.
#' @param fileVariantsAfter062021 the url of the variants after 06 2021.
#' @param fileVariantsAfter012022 the url of the variants after 01 2022.
#'
#' @return A dataframe with the date, the department and the majority variant.
#' @export
ImportMajorityVariant <- function(fileVariantsBefore062021 = "https://www.data.gouv.fr/en/datasets/r/16f4fd03-797f-4616-bca9-78ff212d06e8",
                                  fileVariantsAfter062021 = "https://www.data.gouv.fr/fr/datasets/r/4d3e5a8b-9649-4c41-86ec-5420eb6b530c",
                                  fileVariantsAfter012022 = "https://www.data.gouv.fr/fr/datasets/r/ba8219dc-948c-418a-9116-f792c79c54b8"){
  # import data
  lsVariantsRaw <- LoadVariants(fileVariantsBefore062021 = fileVariantsBefore062021,
                                fileVariantsAfter062021 = fileVariantsAfter062021,
                                fileVariantsAfter012022 = fileVariantsAfter012022,
                                addVariantsBefore062021 = FALSE)

  res <- dplyr::bind_rows(lsVariantsRaw) %>%
    replace(is.na(.), 0) %>%
    distinct() %>%
    mutate(wild = 100 - Prc_susp_501Y_V1 - Prc_susp_501Y_V2_3 - Prc_susp_IND - tx_A1 - tx_B1 - tx_C1 - tx_D1,
           wild = if_else(wild < 0, 0, wild)) %>%
    tidyr::pivot_longer(cols = Prc_susp_501Y_V1:wild, names_to = "Majority_variant") %>%
    mutate(Majority_variant = factor(Majority_variant,
                                     levels = c("Prc_susp_IND",
                                                "wild",
                                                "Prc_susp_501Y_V1",
                                                "Prc_susp_501Y_V2_3",
                                                "tx_A1",
                                                "tx_B1",
                                                "tx_C1",
                                                "tx_D1"),
                                     labels = c("NOID",
                                                "wild",
                                                "Alpha",
                                                "Beta_or_Gamma",
                                                "E484K",
                                                "E484Q",
                                                "L452R",
                                                "Omicron"))) %>%
    mutate(Majority_variant_num = value+as.numeric(Majority_variant)/10^4,
           Majority_variant = as.character(Majority_variant)) %>%
    group_by(DATA_DAY, dep) %>%
    slice_max(order_by = Majority_variant_num, n = 1) %>%
    select(-Majority_variant_num, -value) %>%
    rename("DATE" = "DATA_DAY")

  return(res)
}
