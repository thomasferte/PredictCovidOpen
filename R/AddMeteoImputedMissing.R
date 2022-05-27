#' AddMeteoImputedMissing
#'
#' @description Create a dataset with imputed meteo dataset
#'
#' @param weather_data_dep The departement weather dataset. Usually from PredictCovid::weather_data_dep.
#' @param weather_data_reg The region weather dataset. Usually from PredictCovid::weather_data_reg.
#'
#' @return The imputed dataframe with meteo data.
#' @export
#' @import worldmet
#' @importFrom mice mice complete
#'
#' @examples
#' \dontrun{
#'
#' df_hospitPCRDMMeteo <- AddMeteoImputedMissing(weather_data_dep = PredictCovid::weather_data_dep,
#'                                               weather_data_reg = PredictCovid::weather_data_reg)
#' }
AddMeteoImputedMissing <- function(weather_data_dep,
                                   weather_data_reg){
  varWeather <- c("t.mean", "precip", "RH.mean", "AH.mean", "IPTCC.mean", "ws.mean", "dewpoint.mean")

  ## import departement data
  dfMeteoDep <- weather_data_dep %>%
    mutate(code_insee = if_else(code_insee == "69M", "69", code_insee)) %>%
    # for 69 dep, take only 69M (metropole Lyon)
    filter(code_insee != "69D") %>%
    rename("dep" = "code_insee") %>%
    left_join(PredictCovid::df_dptRegion) %>%
    select(-c(Population, label_insee))

  ## import region data
  dfMeteoReg <- weather_data_reg %>%
    rename("reg" = "code_insee") %>%
    rename_at(.vars = varWeather,
              .funs = function(x) paste0("reg_", x)) %>%
    select(-label_insee)

  ## create department dataframe with one day from min to max for each departement
  vecDep <- unique(dfMeteoDep$dep)
  vecDate <- seq.Date(from = min(dfMeteoDep$date_day), to = max(dfMeteoDep$date_day), by = "day")
  dfMeteoDepDayly <- tidyr::expand_grid(dep = vecDep, date_day = vecDate) %>%
    left_join(df_dptRegion %>% select(-Population))

  ## merge departement and region data
  dfMeteoDepReg <- dfMeteoDepDayly %>%
    left_join(dfMeteoDep) %>%
    left_join(dfMeteoReg) %>%
    group_by(dep) %>%
    arrange(dep, date_day) %>%
    mutate_at(.vars = varWeather,
              .funs = list(lag1 = function(x) lag(x, n = 1),
                           lag2 = function(x) lag(x, n = 2),
                           lag3 = function(x) lag(x, n = 3),
                           lag4 = function(x) lag(x, n = 4),
                           lag5 = function(x) lag(x, n = 5),
                           lag6 = function(x) lag(x, n = 6),
                           lag7 = function(x) lag(x, n = 7))) %>%
    ungroup()

  miceModel <- mice::mice(data = dfMeteoDepReg, m = 1)

  ImpdfMeteoDepReg <- mice::complete(miceModel, 1) %>%
    select(dep, reg, date_day,
           t.mean, precip, RH.mean,
           AH.mean, IPTCC.mean, ws.mean,
           dewpoint.mean)

  ImpdfMeteoAvg <- ImpdfMeteoDepReg %>%
    group_by(dep, reg) %>%
    arrange(dep, reg, date_day) %>%
    mutate_at(.vars = varWeather,
                 .funs = list(avg14 = function(x) slider::slide_dbl(x, .before = 14, .f = function(y) mean(y, na.rm = T)),
                              avg7 = function(x) slider::slide_dbl(x, .before = 7, .f = function(y) mean(y, na.rm = T)))) %>%
    ungroup() %>%
    select(-all_of(varWeather))

  return(ImpdfMeteoAvg)
}
