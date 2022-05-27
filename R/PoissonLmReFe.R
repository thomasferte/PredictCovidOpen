#' PoissonLmReFe
#'
#' @description Run Poisson and lm with and with dep fixed effect or dep/reg random effect.
#'
#' @param df Data
#' @param param_FORECAST The forecast time
#' @param param_DATE The date from which prediction is made
#' @param featureSelection The variables used for prediction
#'
#' @return A list of three dataframes (Coefficients, predictions and marginal department effect)
#' @export
PoissonLmReFe <- function(df,
                          param_FORECAST,
                          param_DATE,
                          featureSelection){
  df_y <- FctComputeY(df,
                      param_Y = "hosp",
                      param_FORECAST = param_FORECAST,
                      outcome = "hosp")
  ### compute train and test sets
  ls_df <- FctTrainTestSidepDelay(df = df_y,
                                  param_DATE = param_DATE,
                                  delaySIDEP = NULL,
                                  param_VARIATION = F)
  ### feature selection
  dfTrain <- ls_df$df_train$X %>%
    select(all_of(featureSelection)) %>%
    mutate(Outcome = ls_df$df_train$Y,
           SET = "TRAIN")

  dfTest <- ls_df$df_test$X %>%
    select(all_of(featureSelection)) %>%
    mutate(Outcome = ls_df$df_test$Y,
           SET = "TEST")

  dfAll <- bind_rows(dfTrain, dfTest) %>%
    mutate(across(where(is.numeric) & !c(Outcome), .fns = function(x) as.numeric(scale(x))))


  dfPredMother <- ls_df$dfTestFull %>%
    select(reg, dep, outcome, outcomeDate)

  lsPred <- list()

  ### train classical models
  # formula
  noRegFeatures <- featureSelection[featureSelection != "reg"]
  formFe <- paste0("Outcome ~ ", paste0(noRegFeatures, collapse = " + "))
  noRegDepFeatures <- noRegFeatures[noRegFeatures != "dep"]
  formRe <- paste0("Outcome ~ ", paste0(noRegDepFeatures, collapse = " + "), " + (1|reg/dep)")
  # lm Fixed effects
  lmFe <- lm(formula = as.formula(formFe), data = dfTrain)
  predlmFe <- predict(lmFe, newdata = dfTest, interval = "prediction")
  lsPred[["lmFe"]] <- dfPredMother %>%
    bind_cols(as.data.frame(predlmFe)) %>%
    rename("Pred" ="fit",
           "PIinf" = "lwr",
           "PIsup" = "upr")
  # lm Random effects
  lmRe <- lme4::lmer(formula = as.formula(formRe), data = dfAll %>% filter(SET == "TRAIN"))
  lsPred[["lmRe"]] <- dfPredMother %>%
    mutate(Pred = predict(lmRe, newdata = dfAll %>% filter(SET == "TEST")))
  ReIntercept <- lme4::fixef(lmRe)["(Intercept)"]
  # lm scaled => for intercept comparisons
  lmFeScaled <- lm(formula = as.formula(formFe), data = dfAll %>% filter(SET == "TRAIN"))
  predlmFeScaled <- predict(lmFeScaled, newdata = dfAll %>% filter(SET == "TEST"), interval = "prediction")
  lsPred[["lmFeScaled"]] <- dfPredMother %>%
    bind_cols(as.data.frame(predlmFeScaled)) %>%
    rename("Pred" ="fit",
           "PIinf" = "lwr",
           "PIsup" = "upr")

  # compare dep and region effect lm vs lme4
  dfTestDepEffectsRe <- dfAll %>%
    filter(SET == "TEST") %>%
    mutate_if(.predicate = is.numeric, .funs = function(x) 0)
  dfTestDepEffectsFe <- dfTest %>%
    filter(SET == "TEST") %>%
    mutate_if(.predicate = is.numeric, .funs = function(x) 0)

  dfDepMarginalEffect <- dplyr::left_join(x = data.frame(dep = dfTestDepEffectsFe$dep,
                                                         lmFe = predict(lmFe, newdata = dfTestDepEffectsFe)),
                                          y = data.frame(dep = dfTestDepEffectsRe$dep,
                                                         lmRe = predict(lmRe, dfTestDepEffectsRe) - ReIntercept,
                                                         lmFeScaled = predict(lmFeScaled, dfTestDepEffectsRe),
                                                         lmReScaled = predict(lmRe, dfTestDepEffectsRe)),
                                          by = "dep")

  # Poisson Fixed effects
  PoissonFe <- glm(formula = as.formula(formFe), data = dfTrain, family = "poisson")
  lsPred[["PoissonFe"]] <- dfPredMother %>%
    mutate(Pred = predict(PoissonFe, newdata = dfTest, type = "response"))
  # Poisson Random effects
  PoissonRe <- lme4::glmer(formula = as.formula(formRe), data = dfAll %>% filter(SET == "TRAIN"), family = "poisson")
  lsPred[["PoissonRe"]] <- dfPredMother %>%
    mutate(Pred = predict(PoissonRe, newdata = dfAll %>% filter(SET == "TEST"), type = "response"))

  # df coef
  dflmFe <- cbind(Coef = coef(lmFe), confint.default(lmFe)) %>%
    as.data.frame() %>%
    rename("CIlwr" = "2.5 %",
           "CIupr" = "97.5 %") %>%
    tibble::rownames_to_column(var = "Features")
  dfPoissonFe <- cbind(Coef = coef(PoissonFe), confint.default(PoissonFe)) %>%
    as.data.frame() %>%
    rename("CIlwr" = "2.5 %",
           "CIupr" = "97.5 %") %>%
    tibble::rownames_to_column(var = "Features")
  dflmRe <- OutputLme4(lmRe)
  dfPoissonRe <- OutputLme4(PoissonRe)

  dfCoef <- bind_rows(list(lmFe = dflmFe,
                           PoissonFe = dfPoissonFe,
                           lmRe = dflmRe,
                           PoissonRe = dfPoissonRe),
                      .id = "Model")

  ### Merge all predictions
  dfPred <- lsPred %>%
    bind_rows(.id = "Model")

  return(list(dfCoef = dfCoef,
              dfPred = dfPred,
              dfDepMarginalEffect = dfDepMarginalEffect))
}
