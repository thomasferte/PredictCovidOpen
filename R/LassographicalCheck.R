#' LassographicalCheck
#'
#' @description Train a lasso linear regression model and give the selected variables as output depending on the forecast time of interest.
#'
#' @param df The dataframe of interest
#' @param param_FORECAST The forecast length.
#' @param alpha The penalisation constraint as in glmnet (default is 1, lasso penalisation).
#'
#' @return A vector with the non zero variables.
#' @export
#'
LassographicalCheck <- function(df,
                                param_FORECAST = 7,
                                alpha = 1){

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
  modelLasso <- glmnet::cv.glmnet(x = x_train,
                                  y = y_train,
                                  alpha = alpha,
                                  nfolds = 10)

  return(modelLasso)
}
