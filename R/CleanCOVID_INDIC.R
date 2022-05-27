#' CleanCOVID_INDIC
#'
#' @description Clean data from RqCovid_Indic table
#'
#' @param RqCOVID_INDIC The table from COVID_INDIC table.
#'
#' @return A cleaned dataframe
#' @export
CleanCOVID_INDIC <- function(RqCOVID_INDIC){
  dfTERMClean <- RqCOVID_INDIC %>%
    select(TERM) %>%
    distinct() %>%
    mutate(TERM_CLEAN = janitor::make_clean_names(TERM))

  dfCleanDashboard <- RqCOVID_INDIC %>%
    # clean TERM
    left_join(dfTERMClean, by = "TERM") %>%
    # drop type
    dplyr::select(-c(TERM, COUNT_AVG_7DAYS, PERCENT_AVG_7DAYS)) %>%
    # distribute the value columns
    tidyr::pivot_longer(cols = c("COUNT", "PERCENT"),
                        names_to = "COUNT_TYPE",
                        values_to = "VALUES") %>%
    # concatenate DOMAIN, TERM_CLEAN and COUNT_TYPE
    mutate(START_DATE = as.Date(START_DATE),
           FEATURE = paste0(DOMAIN, "_", TERM_CLEAN, "_", COUNT_TYPE),
           VALUES = as.numeric(VALUES)) %>%
    select(START_DATE, FEATURE, VALUES) %>%
    # pivot to large format
    tidyr::pivot_wider(names_from = "FEATURE", values_from = "VALUES") %>%
    # remove NA only columns
    select(where(function(x) any(!is.na(x)))) %>%
    # replace NA by 0 for PCR_INTRA_EXTRA columns
    mutate(across(where(is.numeric),
                  ~replace(.x, is.na(.x), 0)))
  return(dfCleanDashboard)
}

