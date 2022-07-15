#' cvFrechetRf_slarTask
#'
#' @description Compute cross validated performance of Frechet randomforest model adapted to slurm curta
#'
#' @param df Dataframe
#' @param char_colDate The date column name (default is "DATE")
#' @param ntree Number of tree for randomforest (default is 100)
#' @param slar_taskid Slurm task id
#' @param dbl_curvePastWindow Curve past window (default is 14)
#' @param ncores cores number (default is 2)
#' @param subset_training learn on 1 out of ten days (default is F). If TRUE, learning will be faster but possibly less accurate
#' @param param_YTYPE a character vector of length 1 indicating the type of Y variable (scalar or curve)
#' @param param_FORECAST a dbl vector of length 1 indicating the forecast time
#' @param param_DATE a date vetor of length 1 indicating the date of which test prediction is made
#' @param param_FE a character vector describing if feature ingeneered variables should be used or not
#' @param delaySIDEP a numeric scalar, SIDEP delay with CHU, 0 (default) if no delay. Usually there are 4 days delay.
#'
#'
#' @return A list of two dataframe: cross validated tree predictions and variables importance
#' @import lubridate
# #' @importFrom  randomForest randomForest
#' @importFrom stats predict
#' @importFrom stats rnorm
# #' @import FrechForest
#' @export
cvFrechetRf_slarTask <- function(df,
                                 char_colDate = "DATE",
                                 slar_taskid,
                                 dbl_curvePastWindow = 14,
                                 ncores = 2,
                                 ntree = 100,
                                 subset_training = F,
                                 param_YTYPE,
                                 param_FORECAST,
                                 param_DATE,
                                 param_FE,
                                 delaySIDEP = 0){

  ## generate Y depending on forecast
  df_Y <- df %>%
    group_by(dep) %>%
    arrange(dep, DATE) %>%
    mutate(outcome = lead(x = hosp, n = param_FORECAST),
           outcomeDate = lead(x = get(char_colDate), n = param_FORECAST),
           DATE = as.POSIXct(DATE)) %>%
    ungroup()

  ## generate X depending on feature engineering
  if(param_FE == "fe"){
    df_X <- df_Y
  } else if(param_FE == "nofe"){
    df_X <- df_Y %>%
      select(-grep(colnames(df_Y), pattern = "_fct_", value = T))
  }

  ## derive variables from user variables
  minDate <- lubridate::as_date(min(df %>% pull(char_colDate)))
  # vector of date of training set
  vec_Date <- as.Date(minDate:param_DATE, origin = "1970-01-01")
  vec_TrainDate_chu <- vec_Date[vec_Date <= param_DATE - lubridate::days(param_FORECAST)]
  vec_TrainDate_sidep <- vec_Date[vec_Date <= param_DATE - lubridate::days(param_FORECAST) - delaySIDEP]
  ## generate training curve and Y scalar
  if(subset_training){
    dbl_maxsamp <- floor(length(vec_Date)/10)
    dbl_intercept <- length(vec_Date) - dbl_maxsamp*10
    vec_Date <- vec_Date[c((1:dbl_maxsamp)*10+dbl_intercept)]
  }

  # date_i <- vec_Date[1]
  ## generate X and Y sets
  ls_XYtrain <- sapply(vec_Date,
                       simplify = F,
                       function(date_i){
                         # add unique dep day identifier
                         df_i <- df_X %>%
                           mutate(depNum = paste0(dep, "_", date_i),
                                  date_i = date_i)
                         # Y scalar
                         Yscalar_i <- df_i %>%
                           filter(DATE == date_i) %>%
                           select(dep, depNum, date_i, DATE, outcome, outcomeDate)
                         # Y curve
                         Ycurve_i <- df_i %>%
                           select(dep, depNum, date_i, DATE, hosp) %>%
                           filter(DATE > date_i,
                                  DATE <= (date_i+param_FORECAST)) %>%
                           mutate(time = as.numeric(round(difftime(DATE, date_i, units = "days"))))


                         # X curve
                         Xcurve_i <- df_i %>%
                           filter(DATE <= date_i,
                                  DATE >= (date_i-dbl_curvePastWindow)) %>%
                           mutate(time = as.numeric(round(difftime(DATE, date_i, units = "days"))),
                                  time = time + abs(min(time))) %>%
                           select(-c(reg, dep, DATE, WEEKDAY, Population, outcome, outcomeDate, TIME))

                         # X scalar
                         Xscalar_i <- df_i %>%
                           filter(DATE == date_i) %>%
                           select(depNum, date_i, Population, TIME)

                         # X factor
                         Xfactor_i <- df_i %>%
                           filter(DATE == date_i) %>%
                           select(depNum, date_i, reg, dep, WEEKDAY)


                         return(list(Yscalar = Yscalar_i,
                                     Ycurve = Ycurve_i,
                                     Xcurve = Xcurve_i,
                                     Xscalar = Xscalar_i,
                                     Xfactor = Xfactor_i))
                       })
  names(ls_XYtrain) <- as.character(vec_Date)

  ## transform the list to a list of 4 dataframe
  df_Yscalar <- lapply(ls_XYtrain, function(x) x$Yscalar) %>% bind_rows()
  # get a numeric id for each depNum
  df_IdDepNum <- df_Yscalar %>%
    select(depNum) %>%
    mutate(IdDepNum = as.numeric(as.factor(depNum)))

  df_Yscalar <- df_Yscalar %>% left_join(df_IdDepNum)
  df_Ycurve <- lapply(ls_XYtrain, function(x) x$Ycurve) %>% bind_rows() %>% left_join(df_IdDepNum)
  df_Xcurve <- lapply(ls_XYtrain, function(x) x$Xcurve) %>% bind_rows() %>% left_join(df_IdDepNum)
  df_Xscalar <- lapply(ls_XYtrain, function(x) x$Xscalar) %>% bind_rows() %>% left_join(df_IdDepNum)
  df_Xfactor <- lapply(ls_XYtrain, function(x) x$Xfactor) %>% bind_rows() %>% left_join(df_IdDepNum)

  ## train set
  df_trainXscalar <- df_Xscalar %>%
    filter((grepl(depNum, pattern = "CHU") & date_i %in% vec_TrainDate_chu) |
             (grepl(depNum, pattern = "SIDEP") & date_i %in% vec_TrainDate_sidep))
  ls_trainXscalar <- list(id = df_trainXscalar$IdDepNum,
                          X = df_trainXscalar %>% select(Population, TIME) %>% as.data.frame())

  df_trainXfactor <- df_Xfactor %>%
    filter((grepl(depNum, pattern = "CHU") & date_i %in% vec_TrainDate_chu) |
             (grepl(depNum, pattern = "SIDEP") & date_i %in% vec_TrainDate_sidep))
  ls_trainXfactor <- list(id = df_trainXfactor$IdDepNum,
                          X = df_trainXfactor %>% select(reg, dep, WEEKDAY) %>% as.data.frame())

  df_trainXcurve <- df_Xcurve %>%
    filter((grepl(depNum, pattern = "CHU") & date_i %in% vec_TrainDate_chu) |
             (grepl(depNum, pattern = "SIDEP") & date_i %in% vec_TrainDate_sidep))
  ls_trainXcurve <- list(id = df_trainXcurve$IdDepNum,
                         time = df_trainXcurve$time,
                         X = df_trainXcurve %>% select(starts_with(match = "P_")|starts_with(match = "TESTED_")|starts_with(match = "hosp"))%>% as.data.frame())

  df_trainYcurve <- df_Ycurve %>%
    filter((grepl(depNum, pattern = "CHU") & date_i %in% vec_TrainDate_chu) |
             (grepl(depNum, pattern = "SIDEP") & date_i %in% vec_TrainDate_sidep))
  ls_trainYcurve <- list(type = "curve",
                         time = df_trainYcurve$time,
                         Y = df_trainYcurve$hosp,
                         id = df_trainYcurve$IdDepNum)

  df_trainYscalar <- df_Yscalar %>%
    filter((grepl(depNum, pattern = "CHU") & date_i %in% vec_TrainDate_chu) |
             (grepl(depNum, pattern = "SIDEP") & date_i %in% vec_TrainDate_sidep))
  ls_trainYscalar <- list(type = "scalar",
                         Y = df_trainYscalar$outcome,
                         id = df_trainYscalar$IdDepNum)

  ## test set
  df_testXscalar <- df_Xscalar %>%
    filter((grepl(depNum, pattern = "CHU") & date_i == param_DATE) |
             (grepl(depNum, pattern = "SIDEP") & date_i == param_DATE - lubridate::days(delaySIDEP)))
  ls_testXscalar <- list(id = df_testXscalar$IdDepNum,
                          X = df_testXscalar %>% select(Population, TIME) %>% as.data.frame())

  df_testXfactor <- df_Xfactor %>%
    filter((grepl(depNum, pattern = "CHU") & date_i == param_DATE) |
             (grepl(depNum, pattern = "SIDEP") & date_i == param_DATE - lubridate::days(delaySIDEP)))
  ls_testXfactor <- list(id = df_testXfactor$IdDepNum,
                          X = df_testXfactor %>% select(reg, dep, WEEKDAY) %>% as.data.frame())


  df_testXcurve <- df_Xcurve %>%
    filter((grepl(depNum, pattern = "CHU") & date_i == param_DATE) |
             (grepl(depNum, pattern = "SIDEP") & date_i == param_DATE - lubridate::days(delaySIDEP)))
  ls_testXcurve <- list(id = df_testXcurve$IdDepNum,
                         time = df_testXcurve$time,
                         X = df_testXcurve %>% select(starts_with(match = "P_")|starts_with(match = "TESTED_")|starts_with(match = "hosp")) %>% as.data.frame())

  df_testYcurve <- df_Ycurve %>%
    filter((grepl(depNum, pattern = "CHU") & date_i == param_DATE) |
             (grepl(depNum, pattern = "SIDEP") & date_i == param_DATE - lubridate::days(delaySIDEP)))
  ls_testYcurve <- list(type = "curve",
                         time = df_testYcurve$time,
                         Y = df_testYcurve$hosp,
                         id = df_testYcurve$IdDepNum)

  df_testYscalar <- df_Yscalar %>%
    filter((grepl(depNum, pattern = "CHU") & date_i == param_DATE) |
             (grepl(depNum, pattern = "SIDEP") & date_i == param_DATE - lubridate::days(delaySIDEP)))
  ls_testYscalar <- list(type = "scalar",
                          Y = df_testYscalar$outcome,
                          id = df_testYscalar$IdDepNum)

  ## train model

  if(param_YTYPE == "scalar"){
    m1_frechetrf <- FrechForest::FrechForest(Curve = ls_trainXcurve,
                                             Scalar = ls_trainXscalar,
                                             Factor = ls_trainXfactor,
                                             Y = ls_trainYscalar,
                                             ncores = ncores,
                                             ERT = T,
                                             ntree = ntree)

    df_predictions <- df_testYscalar %>%
      select(-date_i) %>%
      mutate(PREDICTION = predict(m1_frechetrf,
                                  Curve = ls_testXcurve,
                                  Scalar = ls_testXscalar,
                                  Factor = ls_testXfactor))

  } else if(param_YTYPE == "curve"){
    m1_frechetrf <- FrechForest::FrechForest(Curve = ls_trainXcurve,
                                             Scalar = ls_trainXscalar,
                                             Factor = ls_trainXfactor,
                                             Y = ls_trainYcurve,
                                             ncores = ncores,
                                             ERT = T,
                                             ntree = ntree)

    df_frechpred <- predict(m1_frechetrf,
                            Curve = ls_testXcurve,
                            Scalar = ls_testXscalar,
                            Factor = ls_testXfactor) %>%
      rename("time" = "times",
             "IdDepNum" = "ID",
             "PREDICTION" = "traj") %>%
      full_join(df_testYcurve %>% select(IdDepNum, hosp, time)) %>%
      left_join(df_testYcurve %>% select(IdDepNum, dep, depNum, date_i) %>% distinct()) %>%
      rename("outcome" = "hosp",
             "DATE" = "date_i") %>%
      mutate(outcomeDate = DATE + lubridate::days(round(time))) %>%
      select(-time) %>%
      group_by(IdDepNum, dep, depNum, DATE, outcomeDate) %>%
      summarise(PREDICTION = mean(PREDICTION, na.rm = T),
                outcome = mean(outcome, na.rm = T)) %>%
      ungroup()

  } else {
    errorCondition(message = "param_TYPE should be either scalar or curve")
  }

  ## var importance
  df_varimp <- data.frame(Variables = c(colnames(ls_trainXcurve$X),
                                        colnames(ls_trainXfactor$X),
                                        colnames(ls_trainXscalar$X)),
                          Importance = c(m1_frechetrf$Importance$Curve,
                                         m1_frechetrf$Importance$Factor,
                                         m1_frechetrf$Importance$Scalar))

  ## save results
  return(list(df_predictions = df_predictions,
              df_varimp = df_varimp))
}
