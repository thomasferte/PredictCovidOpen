#' Dataframe of adjacent departement
#'
#'A dataset containing the departement and corresponding adjacent ones
#'
#' @format A data frame with 503 rows and 2 variables
#'
#'@examples
#' ##### script generating meteo dataframe
#' \dontrun{
#' dfLimitrophe <- readxl::read_xlsx("extdata/limitrophes_dep/dep_limitrophes.xlsx") %>%
#' mutate_all(.funs = as.character) %>%
#'   janitor::clean_names() %>%
#'   tidyr::pivot_longer(cols = voisin_1:voisin_8, values_to = "adjacent") %>%
#'   select(-name) %>%
#'   filter(!is.na(adjacent)) %>%
#'   mutate(departement = if_else(nchar(departement) == 1,
#'                                paste0("0", departement),
#'                                departement))
#' save(dfLimitrophe, file = "data/dfLimitrophe.rdata")
#' }
"dfLimitrophe"
