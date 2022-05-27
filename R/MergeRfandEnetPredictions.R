#' MergeRfandEnetPredictions
#'
#' @description Merge Elastic net and random forest predictions
#'
#' @param df_test The test dataframe
#' @param lsmodelEnet The list from trainElasticNet()
#' @param lsmodelRf The list from trainRf()
#' @param OUTCOME The OUTCOME must be one of HOSP or IN_HOSP
#' @param FORECAST The FORECAST time parameter
#'
#' @return The predictions df
#' @export
MergeRfandEnetPredictions <- function(df_test,
                                      lsmodelEnet,
                                      lsmodelRf,
                                      OUTCOME,
                                      FORECAST){
  if(is.null(lsmodelEnet)){
    vecPRED_ENET_raw <- NA
  } else {
    vecPRED_ENET_raw <- lsmodelEnet$pred
  }

  if(is.null(lsmodelRf)){
    vecPRED_RF0.5_raw <- NA
    vecPRED_RF0.025_raw <- NA
    vecPRED_RF0.975_raw <- NA
  } else {
    vecPRED_RF0.5_raw <- lsmodelRf$pred$predictions[,"quantile= 0.5"]
    vecPRED_RF0.025_raw <- lsmodelRf$pred$predictions[,"quantile= 0.025"]
    vecPRED_RF0.975_raw <- lsmodelRf$pred$predictions[,"quantile= 0.975"]
  }

  ## predictions
  dfPredictions <- df_test %>%
    select(START_DATE, outcome, outcomeRef, outcomeDate, CHU_HOSP, CHU_HOSP_rolDeriv7, IN_HOSP_in_COUNT, IN_HOSP_in_COUNT_rolMean) %>%
    mutate(PRED_ENET_raw = vecPRED_ENET_raw,
           PRED_RF0.5_raw = vecPRED_RF0.5_raw,
           PRED_RF0.025_raw = vecPRED_RF0.025_raw,
           PRED_RF0.975_raw = vecPRED_RF0.975_raw) %>%
    rename("outcome_raw" = "outcome",
           "outcome" = "outcomeRef")
  # rescale predictions in the hospitalisation scale
  if(OUTCOME == "HOSP"){
    dfPredictions <- dfPredictions %>%
      mutate(PRED_ENET = PRED_ENET_raw + CHU_HOSP,
             PRED_RF0.5 = PRED_RF0.5_raw + CHU_HOSP,
             PRED_RF0.025 = PRED_RF0.025_raw + CHU_HOSP,
             PRED_RF0.975 = PRED_RF0.975_raw + CHU_HOSP)
  } else if(OUTCOME %in% c("IN_HOSP_in_COUNT_rolMean", "IN_HOSP", "RAW_HOSP")){
    dfPredictions <- dfPredictions %>%
      mutate(PRED_ENET = PRED_ENET_raw,
             PRED_RF0.5 = PRED_RF0.5_raw,
             PRED_RF0.025 = PRED_RF0.025_raw,
             PRED_RF0.975 = PRED_RF0.975_raw)
  } else if(OUTCOME == "HOSP_DERIV"){
    dfPredictions <- dfPredictions %>%
      mutate(PRED_ENET = PRED_ENET_raw + (CHU_HOSP + FORECAST*CHU_HOSP_rolDeriv7),
             PRED_RF0.5 = PRED_RF0.5_raw + (CHU_HOSP + FORECAST*CHU_HOSP_rolDeriv7),
             PRED_RF0.025 = PRED_RF0.025_raw + (CHU_HOSP + FORECAST*CHU_HOSP_rolDeriv7),
             PRED_RF0.975 = PRED_RF0.975_raw + (CHU_HOSP + FORECAST*CHU_HOSP_rolDeriv7))
  } else{
    stop("OUTCOME is unknown")
  }
  return(dfPredictions)
}
