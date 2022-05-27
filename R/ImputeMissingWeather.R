#' ImputeMissingWeather
#'
#' @description Imputation is first done by providing the mean of the adjacent departement and then using the LOCF.
#'
#' @param df_to_imp Dataframe to impute.
#'
#' @return A list of three : the dataframe of interest and to diagnosis dataframe (before mean adjacent step and before LOCF step)
#' @export
ImputeMissingWeather <- function(df_to_imp){

  vecCol_to_imp <- c("t.mean", "precip", "RH.mean", "AH.mean",
                     "IPTCC.mean", "ws.mean", "dewpoint.mean")

  vecDep_to_imp <- df_to_imp$dep %>% unique()

  dfImputedDep <- pbapply::pblapply(vecDep_to_imp,
                                    function(dep_i){
                                      vecDepAdjacent <- dfLimitrophe %>%
                                        filter(departement == dep_i) %>%
                                        pull(adjacent)

                                      dfres <- df_to_imp %>%
                                        filter(dep %in% c(dep_i, vecDepAdjacent)) %>%
                                        group_by(DATE) %>%
                                        summarise(across(all_of(vecCol_to_imp),
                                                         function(x) mean(x, na.rm = TRUE))) %>%
                                        ungroup() %>%
                                        mutate(dep = dep_i)

                                      return(dfres)
                                    }) %>%
    bind_rows()

  dfAdjacentTemp <- df_to_imp %>%
    inner_join(dfImputedDep, by = c("dep", "DATE")) %>%
    mutate(t.mean = coalesce(t.mean.x, t.mean.y),
           precip = coalesce(precip.x, precip.y),
           RH.mean = coalesce(RH.mean.x, RH.mean.y),
           AH.mean = coalesce(AH.mean.x, AH.mean.y),
           IPTCC.mean = coalesce(IPTCC.mean.x, IPTCC.mean.y),
           ws.mean = coalesce(ws.mean.x, ws.mean.y),
           dewpoint.mean = coalesce(dewpoint.mean.x, dewpoint.mean.y)) %>%
    select(-all_of(grep(colnames(.), pattern = "\\.x$|\\.y$", value = TRUE)))

  dfImputed <- dfAdjacentTemp %>%
    group_by(dep) %>%
    arrange(dep, DATE) %>%
    mutate_if(.predicate = is.numeric,
              .funs = function(x) ifelse(is.nan(x), NA, x)) %>%
    tidyr::fill(all_of(vecCol_to_imp))

  # check consecutive NA
  dfDiagBeforeAdjacentImputation <- ConsecutiveNa(df = df_to_imp,
                                                  vars_to_check = vecCol_to_imp)

  dfDiagBeofreLOCF <- ConsecutiveNa(df = dfAdjacentTemp,
                                    vars_to_check = vecCol_to_imp)

  return(list(dfImputed = dfImputed,
              Diag = list(dfDiagBeforeAdjacentImputation = dfDiagBeforeAdjacentImputation,
                          dfDiagBeofreLOCF = dfDiagBeofreLOCF)))
}

