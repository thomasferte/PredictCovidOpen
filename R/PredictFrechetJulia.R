#' PredictFrechetJulia
#'
#' @description Make Frechet random forest prediction and get the prediction dataframe (variable importance is not supported), Julia implementation
#'
#' @param dfOutcomeEngineered The dataframe
#' @param dbl_curvePastWindow The frechet window
#' @param date The date of test set
#' @param ntree Nb of frechet tree to build
#' @param ncores Nb of cores to use
#' @param boot should bootstrap sampling be performed on train set
#'
#' @return A list of two dataframe: predictions and feature importance
#' @export
PredictFrechetJulia <- function(dfOutcomeEngineered,
                                dbl_curvePastWindow = 14,
                                date,
                                ntree = 100,
                                ncores = 1,
                                boot = FALSE){
  print(paste0(Sys.time(), " Start PredictFrechetJulia"))
  ## get curves from dataframe
  dfDateFrechet <- dfOutcomeEngineered %>%
    select(outcomeDate, START_DATE, outcome) %>%
    tibble::rowid_to_column(var = "idFrechet") %>%
    mutate(set = if_else(outcomeDate <= date, "train", NA_character_),
           set = if_else(START_DATE == date, "test", set)) %>%
    filter(!is.na(set))

  # bootstrap train set
  if(boot){
    dfBootTrain <- dfDateFrechet %>%
      filter(set == "train") %>%
      slice_sample(prop = 1, replace = TRUE) %>%
      select(-idFrechet) %>%
      tibble::rowid_to_column(var = "idFrechet")

    dfDateFrechet <- dfDateFrechet %>%
      filter(set == "test") %>%
      bind_rows(dfBootTrain)
  }
  # dfOutcomeEngineered <- dfOutcomeEngineered %>% select(-starts_with("URG"), -starts_with("GRP"), -starts_with("FRACP"), -starts_with("SAMU"), -starts_with("P"))
  # generate X and Y sets
  vecFactorFeature <- dfOutcomeEngineered %>%
    select_if(.predicate = function(x) is.character(x)|is.factor(x)) %>%
    colnames()

  df_Xcurve <- expand.grid(idFrechet = dfDateFrechet$idFrechet,
                           time = 0:(dbl_curvePastWindow-1)) %>%
    left_join(dfDateFrechet %>%
                select(idFrechet, START_DATE, set),
              by = "idFrechet") %>%
    mutate(START_DATE = START_DATE - time) %>%
    left_join(dfOutcomeEngineered, by = "START_DATE") %>%
    rename(id = idFrechet) %>%
    select(-c(outcomeRef, START_DATE, outcomeDate, outcome)) %>%
    na.omit() %>%
    fastDummies::dummy_cols(select_columns = vecFactorFeature,
                            remove_first_dummy = T,
                            remove_selected_columns = TRUE)

  df_Yscalar <- dfDateFrechet %>%
    rename(id = idFrechet) %>%
    select(id, outcome, set)

  # train and test sets
  print(paste0(Sys.time(), " Separate train and test sets"))
  lsTrainTest <- sapply(X = c("train", "test"),
                        simplify = FALSE,
                        FUN = function(selectedSet){
                          dfXcurveSelect <- df_Xcurve %>%
                            filter(set == selectedSet) %>%
                            select(-set) %>%
                            # remove incomplete anteriority observations
                            group_by(id) %>%
                            mutate(nObs = n()) %>%
                            ungroup() %>%
                            filter(nObs == dbl_curvePastWindow) %>%
                            select(-nObs) %>%
                            arrange(id, time)

                          dfXcurveRes <- dfXcurveSelect %>%
                            FromDfToArray()

                          vecYscalarRes <- df_Yscalar %>%
                            filter(set == selectedSet,
                                   id %in% unique(dfXcurveSelect$id)) %>%
                            arrange(id) %>%
                            pull(outcome)

                          return(list(dfXcurve = dfXcurveRes,
                                      vecYscalar = vecYscalarRes))
                        })

  ## train frechet RF
  print(paste0(Sys.time(), " Train Frechet RF"))
  JuliaCall::julia_library("ExtraFrech")
  JuliaCall::julia_library("Distances")
  JuliaCall::julia_assign("arX", lsTrainTest$train$dfXcurve)
  JuliaCall::julia_assign("arY", lsTrainTest$train$vecYscalar)
  JuliaCall::julia_assign("mtry", as.integer(round(dim(lsTrainTest$train$dfXcurve)[3]/3)))
  juliaformTrain <- paste0("m1_frechetrf = ExtraFrechetRF(arX, arY, mtry, ", ntree, ", 3, false, 1, Euclidean())")
  m1_frechetrf <- JuliaCall::julia_eval(juliaformTrain)

  ## test frechet RF
  message("-- Frechet training Ok --")
  print(paste0(Sys.time(), " Predict Frechet RF"))
  JuliaCall::julia_assign("arXtest", lsTrainTest$test$dfXcurve)
  juliaformTrain <- paste0("pred_rf(m1_frechetrf, arXtest, arX)")
  frechPred <- JuliaCall::julia_eval(juliaformTrain)

  ## clean predictions
  print(paste0(Sys.time(), " Clean predictions"))
  dfPredictions <- dfDateFrechet %>%
    filter(set == "test") %>%
    select(-set, -idFrechet) %>%
    mutate(dep = "CHU_BDX",
           PRED = frechPred)

  return(list(dfPred = dfPredictions,
              dfImportance = NULL))
}
