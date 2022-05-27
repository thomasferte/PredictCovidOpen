#' PredictionPublicationFromHyperparam
#'
#' @description Fonction to train model and obtain predictions and variable importance for publication.
#'
#' @param span The span smoothing
#' @param model The statistical model ('enet' or 'poisson' or 'rf')
#' @param df The data
#' @param features The features to use
#' @param forecast The forecast time
#' @param date The date of prediction
#' @param outcomeCol The outcome column
#' @param rolderiv Should the ComputerolMeanMaxMinDeriv be computed
#' @param ntreeFrechet Nb of trees for Frechet
#' @param ncoresFrechet Nb of cores for Frechet
#' @param boot should bootstrap sampling be performed on train set
#' @param frechet_julia Should julia implementation of Frechet random forest be used (default is FALSE)
#'
#' @return A list of length 2 with predictions and importance.
#' @export
PredictionPublicationFromHyperparam <- function(span = 7,
                                                model = "enet",
                                                df,
                                                features,
                                                forecast = 7,
                                                rolderiv = TRUE,
                                                date = as.Date("2020-10-01"),
                                                outcomeCol = "hosp",
                                                ntreeFrechet = 100,
                                                ncoresFrechet = 1,
                                                boot = FALSE,
                                                frechet_julia = FALSE){
  outcome <- "RAW_HOSP"
  ### step1 : feature selection
  dfSelected <- df %>%
    select(any_of(features)) %>%
    ungroup()

  ## group by dep if dep exists
  if("dep" %in% colnames(df)){
    dfSelected <- dfSelected %>%
      group_by(dep)

  }

  ### step2: compute outcome and feature engineering (including smoothing)
  if(span == 0){
    dfOutcomeEngineeredTemp <- ComputeOutcome(df = dfSelected,
                                              OUTCOME = outcome,
                                              FORECAST = forecast,
                                              outcomeCol = outcomeCol)

    if(rolderiv){
      dfOutcomeEngineered <- dfOutcomeEngineeredTemp %>%
        ComputerolMeanMaxMinDeriv(df = .,
                                  excludeTansf = c("START_DATE",
                                                   "outcome",
                                                   "outcomeDate",
                                                   "outcomeRef",
                                                   "Vaccin_1dose",
                                                   "Population")) %>%
        ungroup()
    } else {
      dfOutcomeEngineered <- dfOutcomeEngineeredTemp %>%
        ungroup()
    }

  } else {
    dfOutcomeEngineered <- ComputeOutcomeRef(df = dfSelected,
                                             OUTCOME = outcome,
                                             FORECAST = forecast,
                                             outcomeCol = outcomeCol) %>%
      SmoothAndRolMinMaxMeanRespectDate(df = .,
                                        span_days = span,
                                        DATE = date,
                                        rolderiv = rolderiv,
                                        DATE_colum = "START_DATE",
                                        skip_variables = c("outcomeRef",
                                                           "outcomeDate",
                                                           "Vaccin_1dose",
                                                           "Population")) %>%
      ComputeOutcomeCustom(df = .,
                           OUTCOME = outcome,
                           FORECAST = forecast) %>%
      ungroup()
  }

  ## correct missing outcome Date
  dfOutcomeEngineered <- dfOutcomeEngineered %>%
    mutate(outcomeDate = if_else(is.na(outcomeDate), START_DATE + forecast, outcomeDate))

  ### step3 : transform character and factor features to dummies if model is using glmnet
  if(model %in% c("enet", "poisson")){
    vecFactoColumns <- dfOutcomeEngineered %>%
      select_if(.predicate = function(x) is.factor(x)|is.character(x)) %>%
      colnames()
    if(length(vecFactoColumns) >= 1){
      dfOutcomeEngineered <- fastDummies::dummy_columns(.data = dfOutcomeEngineered,
                                                        select_columns = vecFactoColumns,
                                                        remove_first_dummy = TRUE,
                                                        remove_selected_columns = FALSE)
    }
  }

  ### step4 : separate train and test sets + train model
  if(model == "frechet"){
    if(frechet_julia) {
      lsRes <- PredictFrechetJulia(dfOutcomeEngineered = dfOutcomeEngineered,
                                   dbl_curvePastWindow = 14,
                                   date = date,
                                   ntree = ntreeFrechet,
                                   ncores = ncoresFrechet,
                                   boot = boot)
    } else {
      lsRes <- PredictFrechet(dfOutcomeEngineered = dfOutcomeEngineered,
                              dbl_curvePastWindow = 14,
                              date = date,
                              ntree = ntreeFrechet,
                              ncores = ncoresFrechet,
                              boot = boot)
    }

  } else {
    lsTrainTest <- dfOutcomeEngineered %>%
      SeparateTrainTest(df = .,
                        DATE = date,
                        SELECTION = "")

    lsRes <- TrainModelforPublication(lsTrainTest = lsTrainTest,
                                      model = model,
                                      boot = boot)
  }

  return(lsRes)
}
