#' ArrangeModelbyCompromise
#'
#' @description Find best model (sum of MRE) on all forecast time
#'
#' @param dfPerf The performance dataframe from Dfperformances()
#' @param forecast The forecast times
#'
#' @return A dataframe with model performance arrange by sum of MRE
#' @export
ArrangeModelbyCompromise <- function(dfPerf,
                                     forecast = c(4, 7, 14)){
  res <- dfPerf %>%
    filter(FORECAST %in% forecast) %>%
    group_by(SELECTION, OUTCOME, SPAN, Model) %>%
    mutate(MREcompromise = sum(MRE),
           MAEcompromise = sum(MAE)) %>%
    arrange(MREcompromise) %>%
    ungroup()

  return(res)
}
