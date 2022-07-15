#' CHULmModel
#'
#' @description Train a model on CHU data with SIDEP predictions
#'
#' @param param_FORECAST Forecast time
#' @param param_DATE Date from which prediction is made
#' @param Features Selected features
#' @param dfCHU dataframe with features
#' @param variation should the model work on variation rather than raw number
#' @param mfp should fractional polynomial be used on numeric values
#'
#' @return A list of two dataframes: prediction and coefficients
# #' @import mfp
#' @export
CHULmModel <- function(param_FORECAST,
                       param_DATE,
                       Features,
                       dfCHU,
                       variation = F,
                       mfp = F){
  ### add predictions to variable subset
  if(param_FORECAST == 7){
    vecX <- c("33_SIDEP11", "33_CHU7", Features)
  } else if (param_FORECAST == 14){
    vecX <- c("33_SIDEP18", "33_CHU14", Features)
  } else {
    stop("Error, param_FORECAST should be 7 or 14")
  }

  ### train and test
  dfY <- dfCHU %>%
    arrange(DATE_load) %>%
    mutate(outcome = lead(hosp_33_CHUDEP, n = param_FORECAST),
           outcomeDate = lead(DATE_load, n = param_FORECAST)) %>%
    select(DATE_load, outcome, outcomeDate, all_of(vecX)) %>%
    mutate(outcomeVar = outcome - hosp_33_CHUDEP) %>%
    rename_all(.funs = function(x) gsub(x, pattern = "^33", replacement = "v33"))

  dfTrain <- dfY %>%
    filter(outcomeDate <= param_DATE) %>%
    select(-c(DATE_load, outcomeDate)) %>%
    na.omit()

  dfTest <- dfY %>%
    filter(DATE_load == param_DATE)

  ### model training
  formOutcome <- ifelse(variation, "outcomeVar", "outcome")
  charNumeric <- dfTrain %>% select(-c(outcome, outcomeVar)) %>% select_if(.predicate = is.numeric) %>% colnames()

  if(mfp){
    formLm <- paste0(formOutcome, " ~ ", paste0("fp(", charNumeric, ")", collapse = " + "))
    modelLm <- mfp::mfp(formula = as.formula(formLm), data = dfTrain)

  } else {
    formLm <- paste0(formOutcome, " ~ ", paste0(charNumeric, collapse = " + "))
    modelLm <- lm(formula = as.formula(formLm), data = dfTrain)
  }


  ### Predictions
  vecPredictions <- as.numeric(predict(modelLm, newdata = dfTest, interval = "prediction"))

  if(variation){
    dfPred <- dfTest %>%
      mutate(Prediction = vecPredictions[1] + hosp_33_CHUDEP,
             PIinf = vecPredictions[2] + hosp_33_CHUDEP,
             PIsup = vecPredictions[3] + hosp_33_CHUDEP) %>%
      select(DATE_load, outcome, outcomeDate, starts_with("v33_CHU"), Prediction, PIinf, PIsup)

  } else {
    dfPred <- dfTest %>%
      select(DATE_load, outcome, outcomeDate, starts_with("v33_CHU")) %>%
      mutate(Prediction = vecPredictions[1],
             PIinf = vecPredictions[2],
             PIsup = vecPredictions[3])

  }

  ### Coef
  if(mfp){
    dfCoef <- cbind(coef(modelLm)) %>%
      as.data.frame() %>%
      rename("MeanPred" = "V1")
  } else {
    dfCoef <- cbind(coef(modelLm), confint(modelLm)) %>%
      as.data.frame() %>%
      rename("MeanPred" = "V1",
             "CIinf" = "2.5 %",
             "CIsup" = "97.5 %")
  }

  lsResult <- list(Prediction = dfPred, Coef = dfCoef)

  return(lsResult)
}
