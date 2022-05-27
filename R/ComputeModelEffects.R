#' ComputeModelEffects
#'
#' @description Compute the model parameter effect (value * coefficient)
#'
#' @param model The model of interest
#' @param dfPerf The performance dataframe
#' @param dflsDataGroupImp The ID_param model corresponding
#' @param dflsDataImp The importance merged to dfPerf using dflsDataGroupImp
#' @param dfDashboardRaw The raw features
#' @param DateofInterest The date of interest
#'
#' @return A dataframe with model effect.
#' @export
ComputeModelEffects <- function(model,
                                dfPerf,
                                dflsDataGroupImp,
                                dflsDataImp,
                                dfDashboardRaw,
                                DateofInterest){

  dfBestEnet7d <- model %>%
    select(SELECTION, OUTCOME, SPAN, Model) %>%
    left_join(dfPerf) %>%
    filter(FORECAST == 7) %>%
    mutate(Label = paste0(OUTCOME, "_",
                          SELECTION, "_span",
                          SPAN, "_",
                          Model)) %>%
    select(SELECTION, OUTCOME, FORECAST, SPAN) %>%
    mutate(ImportanceType = "Lasso") %>%
    left_join(dflsDataGroupImp) %>%
    left_join(dflsDataImp) %>%
    filter(Importance != 0,
           DATE == max(DATE))

  dfFeaturesMaxDate <- dfDashboardRaw %>%
    SmoothAndRolMinMaxMeanRespectDate(df = .,
                                      span_days = 14,
                                      DATE = DateofInterest,
                                      DATE_colum = "START_DATE",
                                      skip_variables = NULL) %>%
    filter(START_DATE == DateofInterest) %>%
    select(-START_DATE) %>%
    fastDummies::dummy_cols(select_columns = "CHU_WEEKDAY", remove_selected_columns = T) %>%
    tidyr::pivot_longer(cols = colnames(.), names_to = "Features")

  dfEffectHosp7d <- dfBestEnet7d %>%
    left_join(dfFeaturesMaxDate, by = "Features") %>%
    mutate(Effect = Importance * value) %>%
    select(SELECTION, Features, Importance, value, Effect)

  return(dfEffectHosp7d)
}
