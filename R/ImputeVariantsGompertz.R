#' ImputeVariantsGompertz
#'
#' @description Impute missing variants data with a gompertz law.
#'
#' @param dfVariantsRaw The raw variants dataframe
#' @param datePlateauVariants The max date used for imputation
#'
#' @return A dataset with the imputed values.
#' @export
ImputeVariantsGompertz <- function(dfVariantsRaw,
                                   datePlateauVariants){
  minVariantsDate <- min(dfVariantsRaw$DATA_DAY)

  dfVariantsDateNum <- dfVariantsRaw %>%
    mutate(TIME_VARIANTS = as.numeric(DATA_DAY - minVariantsDate)) %>%
    filter(DATA_DAY <= datePlateauVariants)

  vecDep <- unique(dfVariantsDateNum$dep)

  gompertzFormula <- purrr::map(.x = vecDep, .f = ~paste0("K*exp(log(x0_", .x, "/K)*exp(-a*TIME_VARIANTS))*(dep == '", .x, "')")) %>%
    paste0(collapse = " + ") %>%
    paste0("Prc_susp_501Y_V1_V2_V3_IND ~ ", .) %>%
    as.formula()

  vecStartGompertz <- c(90, 0.1, rep(0.1, length(vecDep)))
  names(vecStartGompertz) <- c("K", "a", paste0("x0_", vecDep))

  vecLowerGompertz <- c(70, 0.01, rep(0.01, length(vecDep)))
  names(vecLowerGompertz) <- c("K", "a", paste0("x0_", vecDep))

  gompertzFit <- nls(formula = gompertzFormula,
                     start = vecLowerGompertz,
                     lower = vecLowerGompertz,
                     algorithm = "port",
                     data=dfVariantsDateNum)

  vecDate <- seq(as.Date("2020-01-01"), max(dfVariantsRaw$DATA_DAY), by = 1)

  dfVariantsImputed <- expand.grid(dep = vecDep, DATA_DAY = vecDate) %>%
    mutate(TIME_VARIANTS = as.numeric(DATA_DAY - minVariantsDate)) %>%
    mutate(PRED_GOMPERTZ = predict(gompertzFit, newdata = .)) %>%
    left_join(dfVariantsRaw, by = c("dep", "DATA_DAY")) %>%
    mutate(Prc_susp_501Y_V1_V2_V3_IND = if_else(is.na(Prc_susp_501Y_V1_V2_V3_IND), PRED_GOMPERTZ, Prc_susp_501Y_V1_V2_V3_IND)) %>%
    select(dep, DATA_DAY, Prc_susp_501Y_V1_V2_V3_IND)

  return(dfVariantsImputed)
}
