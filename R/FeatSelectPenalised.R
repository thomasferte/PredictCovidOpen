#' FeatSelectPenalised
#'
#' @description [Production] Train a lasso linear regression model and give the selected variables as output depending on the forecast time of interest.
#'
#' @param df The dataframe of interest
#' @param param_FORECAST The forecast length.
#' @param alpha The penalisation constraint as in glmnet (default is 1, lasso penalisation).
#' @param nboot Number of bootstrap samples the model should be trained (for robustness).
#'
#' @return A a vetor with the non zero variables.
#' @export
#'
FeatSelectPenalised <- function(df,
                                param_FORECAST = 7,
                                alpha = 1,
                                nboot = 100){

  ### compute the outcome
  ## generate the outcome depending on the forecast time and the type of Y
  df_y <- FctComputeY(df,
                      param_Y = "hosp",
                      param_FORECAST = param_FORECAST,
                      outcome = "hosp") %>%
    na.omit()

  y_train <- df_y$outcome

  x_train <- df_y %>%
    select(-DATE, -outcomeDate, -outcomeModel, -outcome) %>%
    fastDummies::dummy_columns(select_columns = c("dep", "reg", "WEEKDAY"),
                               remove_first_dummy = T,
                               remove_selected_columns = T) %>%
    as.matrix()
  ### train the model
  pb <- txtProgressBar(min = 0, max = nboot, style = 3)

  dfBetaAll <- sapply(X = seq(1, nboot, by = 1),
                      simplify = F,
                      FUN = function(i){
                        setTxtProgressBar(pb, i)

                        sampleBoot <- sample(x = 1:nrow(x_train), replace = T, size = nrow(x_train))

                        dfBeta <- glmnet::cv.glmnet(x = x_train[sampleBoot,],
                                                    y = y_train[sampleBoot],
                                                    alpha = alpha,
                                                    nfolds = 10) %>%
                          FctVarImpEnet(.) %>%
                          rename("Beta" = "Importance") %>%
                          select(Features, Beta) %>%
                          filter(Features != "(Intercept)") %>%
                          mutate(fold = i)

                        return(dfBeta)
                      }) %>%
    bind_rows()

  return(dfBetaAll)
}
