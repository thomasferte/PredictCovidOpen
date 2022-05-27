#' PredictFrechet
#'
#' @description Make Frechet random forest prediction and get the prediction dataframe (variable importance is not supported)
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
PredictFrechet <- function(dfOutcomeEngineered,
                           dbl_curvePastWindow = 14,
                           date,
                           ntree = 100,
                           ncores = 1,
                           boot = FALSE){

  ## get curves from dataframe
  dfDateFrechet <- dfOutcomeEngineered %>%
    select(outcomeDate, START_DATE, outcome) %>%
    distinct() %>%
    filter(START_DATE != min(START_DATE)) %>%
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

  # generate X and Y sets
  ls_XYtrain <- apply(dfDateFrechet, 1,
                      function(row){
                        date_i <- as.Date(row[["outcomeDate"]])
                        idFrechet <- as.numeric(row[["idFrechet"]])
                        dfWindow <- dfOutcomeEngineered %>%
                          filter(outcomeDate <= date_i,
                                 outcomeDate > (date_i-dbl_curvePastWindow)) %>%
                          mutate(time = as.numeric(round(difftime(outcomeDate, date_i, units = "days"))),
                                 time = time + abs(min(time)),
                                 id = idFrechet) %>%
                          select(-all_of(c("outcomeRef", "START_DATE")))
                        # Y scalar
                        Yscalar_i <- dfWindow %>%
                          filter(outcomeDate == date_i) %>%
                          select(all_of(c("id", "outcome")))
                        # X curve
                        Xcurve_i <- dfWindow %>%
                          select(-all_of(c("outcome", "outcomeDate"))) %>%
                          select_if(grepl(x = names(.), pattern = "id")|sapply(., is.numeric))

                        # X factor
                        Xfactor_i <- dfWindow %>%
                          filter(outcomeDate == date_i) %>%
                          select(-all_of(c("outcome", "outcomeDate"))) %>%
                          select_if(grepl(x = names(.), pattern = "^id$")|sapply(., function(x) !is.numeric(x)))

                        return(list(Yscalar = Yscalar_i,
                                    Xcurve = Xcurve_i,
                                    Xfactor = Xfactor_i))
                      })
  # merge X and Y sets
  df_Yscalar <- lapply(ls_XYtrain, function(x) x$Yscalar) %>%
    bind_rows() %>%
    left_join(dfDateFrechet %>% select(set, idFrechet), by = c("id" = "idFrechet")) %>%
    na.omit()
  df_Xcurve <- lapply(ls_XYtrain, function(x) x$Xcurve) %>%
    bind_rows() %>%
    left_join(dfDateFrechet %>% select(set, idFrechet), by = c("id" = "idFrechet")) %>%
    na.omit()
  df_Xfactor <- lapply(ls_XYtrain, function(x) x$Xfactor) %>%
    bind_rows() %>%
    left_join(dfDateFrechet %>% select(set, idFrechet), by = c("id" = "idFrechet")) %>%
    na.omit()

  # train and test sets
  lsTrainTest <- sapply(X = c("train", "test"),
                        simplify = FALSE,
                        FUN = function(selectedSet){
                          df_XcurveSelected <- df_Xcurve %>% filter(set == selectedSet) %>% select(-set)
                          df_XfactorSelected <- df_Xfactor %>% filter(set == selectedSet) %>% select(-set)
                          df_YscalarSelected <- df_Yscalar %>% filter(set == selectedSet) %>% select(-set)

                          ls_Xcurve <- list(time = df_XcurveSelected$time,
                                            id = df_XcurveSelected$id,
                                            X = df_XcurveSelected %>% select(-c(time, id)))

                          ls_Xfactor <- list(id = df_XfactorSelected$id,
                                             X = df_XfactorSelected %>% select(-c(id)))

                          ls_Yscalar <- list(type = "scalar",
                                             Y = df_YscalarSelected$outcome,
                                             id = df_YscalarSelected$id)
                          return(list(ls_Xcurve = ls_Xcurve,
                                      ls_Xfactor = ls_Xfactor,
                                      ls_Yscalar = ls_Yscalar))
                        })

  ## train frechet RF
  m1_frechetrf <- FrechForest::FrechForest(Curve = lsTrainTest$train$ls_Xcurve,
                                           Factor = lsTrainTest$train$ls_Xfactor,
                                           Y = lsTrainTest$train$ls_Yscalar,
                                           ncores = ncores,
                                           ERT = T,
                                           imp = FALSE,
                                           ntree = ntree)

  ## test frechet RF
  message("-- Frechet training Ok --")
  frechPred <- predict(m1_frechetrf,
                       Curve = lsTrainTest$test$ls_Xcurve,
                       Factor = lsTrainTest$test$ls_Xfactor)

  ## clean predictions
  dfPredictions <- dfDateFrechet %>%
    filter(set == "test") %>%
    select(-set) %>%
    mutate(dep = "CHU_BDX",
           PRED = frechPred)

  return(list(dfPred = dfPredictions,
              dfImportance = NULL))

}
