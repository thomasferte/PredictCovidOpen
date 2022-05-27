#' fct_meanCases
#'
#' @description internal function for builddf_modelPCR
#'
#' @param x vector to cumsum lag of
#' @param lagtime deepness of lag
#' @importFrom stats na.omit
#' @return a vector of lag cumsum
fct_meanCases <- function(x, lagtime){
  roll_sum(x = x, lagtime, align = "right", fill = NA)/lagtime
}


#' fct_firstDeriv
#'
#' @description internal function for builddf_modelPCR
#'
#' @param x vector to take derivative of
#' @param lagtime deepness of lag
#'
#' @return a vector of derivatives
#' @export
#'
fct_firstDeriv <- function(x, lagtime){
  return( (x-lag(x = x, n = lagtime))/lagtime )
}


#' builddf_modelPCR
#'
#' @description Build dataset to forecast number of COVID positive cases
#'
#' @param df Base dataframe usually df_pcrAfterMay from datagouv_importData.
#' @param biaswindow Cumulate the information on multiple days to take into account the measurement bias. Default is 7 days.
#' @param forecastwindow The forecast distance for prediction. Default is 7 days.
#'
#' @return A dataframe with forecast the variable to forecast and explanatory variables.
#' @export
#' @importFrom RcppRoll roll_sum
builddf_modelPCR <- function(df,
                             biaswindow = 7,
                             forecastwindow = 7){

  ##### cumul des données par fenêtre de 7 jours
  df_rollPcr <- df %>%
    arrange(dep, DATE) %>%
    group_by(dep, cl_age90) %>%
    mutate_at(.vars = c("P"),
              .funs = list(roll = function(x) roll_sum(x, biaswindow, align = "right", fill = NA))) %>%
    mutate_at(.vars = c("roll"),
              .funs = list(forecast = function(x) lead(x = x, n = forecastwindow))) %>%
    filter(!(is.na(roll) | is.na(forecast)))

  ##### get the Y variable
  df_rollPcrY <- df_rollPcr %>%
    ungroup() %>%
    filter(cl_age90 == "tous \u00E2ges") %>%
    select(dep, DATE, forecast)

  ##### feature ingeenering
  vec_lagTimepoints <- c(1, 3, 7, 14)

  ### create list of those function
  ls_fctMeanCases <- sapply(vec_lagTimepoints,
                            simplify = F,
                            function(lagtime) function(x) fct_meanCases(x = x, lagtime = lagtime))
  names(ls_fctMeanCases) <- paste0("fct_meanCases", vec_lagTimepoints)

  ls_fctFirstDeriv <- sapply(vec_lagTimepoints,
                             simplify = F,
                             function(lagtime) function(x) fct_firstDeriv(x = x, lagtime = lagtime))
  names(ls_fctFirstDeriv) <- paste0("fct_firstDeriv", vec_lagTimepoints)

  df_ModelPcr <- df_rollPcr %>%
    select(reg, dep, DATE, cl_age90, roll) %>%
    group_by(dep, cl_age90) %>%
    mutate_at(.vars = "roll", .funs = c(ls_fctMeanCases, ls_fctFirstDeriv)) %>%
    ungroup() %>%
    select(-roll) %>%
    tidyr::pivot_wider(names_from = cl_age90, values_from = fct_meanCases1:fct_firstDeriv14) %>%
    left_join(df_rollPcrY) %>%
    na.omit()

  return(df_ModelPcr)

}

