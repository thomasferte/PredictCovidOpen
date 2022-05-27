#' ggplotBestModelError
#'
#' @description Plot best model with mean relative and absolute error
#'
#' @param df The best model dataframe
#'
#' @return A list with month error dataframe and plot
#' @export
ggplotBestModelError <- function(df){

  dfResultsMergedPred <- df %>%
    filter(FORECAST %in% c(7, 14)) %>%
    mutate(PRED_ENET = if_else(PRED_ENET <0, 0, PRED_ENET))

  dfMonthPerf <- dfResultsMergedPred %>%
    filter(outcomeDate >= as.Date("2020-08-01")) %>%
    select(outcomeDate, FORECAST, outcome, PRED_ENET) %>%
    mutate(diff = PRED_ENET - outcome,
           relativeDiff = diff/outcome,
           START_DATE = as.Date(paste0(format(outcomeDate, "%Y-%m"), "-15"))) %>%
    group_by(FORECAST, START_DATE) %>%
    summarise(MAE = mean(abs(diff), na.rm = T),
              MRE = mean(abs(relativeDiff), na.rm = T)) %>%
    ungroup() %>%
    tidyr::pivot_longer(cols = c("MRE", "MAE"))

  dfplot <- dfResultsMergedPred %>%
    select(START_DATE, outcome, PRED_ENET, FORECAST) %>%
    tidyr::pivot_longer(cols = c("outcome", "PRED_ENET")) %>%
    bind_rows(dfMonthPerf) %>%
    mutate(name = factor(name,
                         levels = c("outcome", "PRED_ENET", "MAE", "MRE"),
                         labels = c("Observed", "Forecast", "Mean Abs Error", "Mean Rel. Error")),
           facet = name,
           facet = if_else(facet %in% c("Observed", "Forecast"), "Hospitalisations", as.character(facet)))

  plot <- ggplot(mapping = aes(x = START_DATE, y = value, color = name)) +
    geom_line(data = dfplot) +
    geom_point(data = dfplot %>% filter(facet != "Hospitalisations"), mapping = aes(group = START_DATE)) +
    facet_grid(facet ~ FORECAST, scales = "free_y") +
    ggsci::scale_color_jco() +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 90)) +
    labs(x = "Date", y = "Value", color = "") +
    scale_x_date(breaks = "month", date_labels = "%m-%y")

  return(list(dfMonthPerf = dfMonthPerf,
              plot = plot))
}
