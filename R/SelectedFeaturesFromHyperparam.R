#' SelectedFeaturesFromHyperparam
#'
#' @description Select features from hyperparameters
#'
#' @param SELECTION The selection parameter muste be one of the following: VI (user input), VI_7dMaxDeriv (another user input), vecVI_small (a third user input), All (all features are used), One_step (selection is done prior to 10-12-2020), Adaptative (selection is done using lasso and rf, for rf a number of features equals to lasso length of selection is used), Adaptative_3months (same as adaptative but the training of this model is used only on previous 3 months)
#' @param lsFeatures The list of features computed by the hyperparam_chu_dashboard.R script
#' @param df_train The train set
#' @param OUTCOME The OUTCOME must be one of HOSP or IN_HOSP
#' @param FORECAST The forecast time.
#'
#' @return A list with random forest and lasso features selection
#' @export
SelectedFeaturesFromHyperparam <- function(SELECTION,
                                           lsFeatures,
                                           df_train,
                                           OUTCOME,
                                           FORECAST){

  maxTrainingDate <- max(df_train$START_DATE) # max date of training set

  if(SELECTION %in% c("VI", "VI_add_wvv",
                      "VI_7dMaxDeriv", "VI_7dMaxDeriv_add_wvv",
                      "VI_small", "VI_small_add_wvv",
                      "All", "All_add_wvv",
                      "SIDEP")){
    featuresLasso <- lsFeatures[[SELECTION]]

    # exclude variants and vaccin if date is too early
    if(maxTrainingDate < lsFeatures$VaccinDate$date){
      featuresLasso <- featuresLasso[!featuresLasso %in% lsFeatures$VaccinDate$features]
    }
    if(maxTrainingDate < lsFeatures$VariantsDate$date){
      featuresLasso <- featuresLasso[!featuresLasso %in% lsFeatures$VariantsDate$features]
    }

    featuresRf <- featuresLasso

  } else if(SELECTION %in% c("One_step", "One_step_add_wvv")){
    dfOnestepSelection <- lsFeatures$dfVarImportance %>%
      filter(SELECTION == SELECTION,
             OUTCOME == OUTCOME,
             FORECAST == FORECAST)
    featuresLasso <- dfOnestepSelection %>%
      filter(Importance != 0,
             ImportanceType == "Lasso") %>%
      pull(Features)
    featuresRf <- dfOnestepSelection %>%
      filter(ImportanceType == "Rf - Permutation") %>%
      top_n(Importance, n = length(featuresLasso)) %>%
      pull(Features)
  } else if(SELECTION %in% c("Adaptative", "Adaptative_3months",
                             "Adaptative_add_wvv", "Adaptative_3months_add_wvv")){

    if(SELECTION %in% c("Adaptative", "Adaptative_3months")){

      df_train <- df_train %>%
        select(START_DATE, outcomeDate, outcomeRef, outcome, lsFeatures$All) # remove vaccin, variants and weather if selection is not add_wvv

    } else if(SELECTION %in% c("Adaptative_add_wvv", "Adaptative_3months_add_wvv")){
      # exclude variants and vaccin if date is too early
      if(maxTrainingDate < lsFeatures$VaccinDate$date){
        df_train <- df_train %>%
          select(-lsFeatures$VaccinDate$features)
      }
      if(maxTrainingDate < lsFeatures$VariantsDate$date){
        df_train <- df_train %>%
          select(-lsFeatures$VariantsDate$features)
      }
    }
    ## train lasso and rf and select number of features equal to the number selected by lasso
    dfImportanceTrain <- OneStepFeatureSelection(OUTCOME = OUTCOME,
                                                 FORECAST = FORECAST,
                                                 df = df_train %>% select(-outcomeDate, -outcomeRef))

    featuresLasso <- dfImportanceTrain %>%
      filter(Importance != 0, ImportanceType == "Lasso") %>%
      pull(Features)
    featuresRf <- dfImportanceTrain %>%
      filter(ImportanceType == "Rf - Permutation") %>%
      top_n(Importance, n = length(featuresLasso)) %>%
      pull(Features)
  } else {
    stop("SELECTION unknown")
  }

  return(list(featuresLasso = featuresLasso,
              featuresRf = featuresRf))
}
