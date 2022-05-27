#' CleanCOVID_FEATURES_SAVE_BIS
#'
#' @description Clean the table from COVID_FEATURES_SAVE_BIS
#'
#' @param RqCOVID_FEATURES_SAVE_BIS The table request
#'
#' @return A cleaned table
#' @export
CleanCOVID_FEATURES_SAVE_BIS <- function(RqCOVID_FEATURES_SAVE_BIS){

  maxDateSidep <- RqCOVID_FEATURES_SAVE_BIS %>%
    filter(DEP == "33_SIDEP") %>%
    pull(DATA_DATE) %>%
    max()

  maxDateCHU <- RqCOVID_FEATURES_SAVE_BIS %>%
    filter(DEP == "33_CHU") %>%
    pull(DATA_DATE) %>%
    max()

  sidepDelay <- as.numeric(as.Date(maxDateCHU) - as.Date(maxDateSidep))

  if(sidepDelay < 0) stop("CHU is posterior to SIDEP which is not expected.")

  df33SIDEP <- RqCOVID_FEATURES_SAVE_BIS %>%
    filter(DEP == "33_SIDEP") %>%
    mutate(START_DATE = as.Date(DATA_DATE) + lubridate::days(sidepDelay), .before = 1) %>%
    select(-c(REG, DEP, SAVE_DATE, DATA_DATE)) %>%
    rename_at(.vars = seq(2, ncol(.), by = 1), .funs = function(x) paste0("GIRONDE_", x))

  df33CHU <- RqCOVID_FEATURES_SAVE_BIS %>%
    filter(DEP == "33_CHU") %>%
    mutate(START_DATE = as.Date(DATA_DATE), .before = 1) %>%
    select(-c(REG, DEP, SAVE_DATE, DATA_DATE)) %>%
    rename_at(.vars = seq(2, ncol(.), by = 1), .funs = function(x) paste0("CHU_", x))

  dfCleanFeatures <- df33SIDEP %>%
    left_join(df33CHU, by = "START_DATE")

  return(dfCleanFeatures)
}
