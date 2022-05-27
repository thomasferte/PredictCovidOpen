#' ggplotModelbyForecast
#'
#' @description Prediction by forecast time
#'
#' @param df A dataframe with predictions
#' @param dfDashboard A raw dataframe
#' @param margin Should 10 and 20\% margin be shown
#' @param trainTestDate Inferior limit of plot default is NULL
#'
#' @return A ggplot object
#' @export
ggplotModelbyForecast <- function(df,
                                  dfDashboard,
                                  margin = FALSE,
                                  trainTestDate = NULL){
  # get the margin
  dfMarginBestCompromise <- df %>%
    filter(FORECAST %in% c(7, 14)) %>%
    select(START_DATE, outcome, outcomeDate, FORECAST, PRED_ENET) %>%
    left_join(dfDashboard %>%
                select(START_DATE,
                       CHU_HOSP)) %>%
    tidyr::pivot_longer(cols = c(outcome, PRED_ENET, CHU_HOSP)) %>%
    mutate(name = factor(name,
                         levels = c("outcome", "CHU_HOSP", "PRED_ENET"),
                         labels = c("Observed", "Constant", "Prediction"))) %>%
    ungroup()

  if(!is.null(trainTestDate)){
    dfMarginBestCompromise <- dfMarginBestCompromise %>% filter(outcomeDate >= trainTestDate)
  }

  dfMarginPercent <- dfMarginBestCompromise %>%
    na.omit() %>%
    filter(name == "Prediction") %>%
    mutate(SUP = if_else(FORECAST == 7, value * 1.1, value * 1.2),
           INF = if_else(FORECAST == 7, value * 0.9, value * 0.8))

  res <- ggplot(dfMarginBestCompromise %>%
                                       group_by(FORECAST) %>%
                                       na.omit() %>%
                                       filter(name != "Constant"|(outcomeDate > min(outcomeDate, na.rm = T)+FORECAST)),
                                     mapping = aes(x = outcomeDate, y = value, color = name, linetype = name))
  if(margin){
    res <- res +
      geom_ribbon(data = dfMarginPercent,
                  mapping = aes(x = outcomeDate, ymin = INF, ymax = SUP, fill = "Margin of error (10% at 7 days, 20% at 14 days)"),
                  color = "white",
                  inherit.aes = FALSE)
  }

  res <- res +
    geom_line() +
    facet_wrap(FORECAST ~ .) +
    scale_color_manual(values = c("#2E86AB", "#113240", "#F24236")) +
    scale_linetype_manual(values = c(1,6,1)) +
    scale_fill_manual(values = "#E1E1E0") +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.box = "vertical") +
    labs(x = "Date", y = "Hospitalisations", color = "", fill = "", linetype = "")

  return(res)
}
