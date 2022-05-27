#' ggplotNPI
#'
#' @description Generate NPI plot from raw data
#'
#' @param dfDashboard data
#'
#' @return A plot of NPI, IPTCC and hospitalisations
#' @export
ggplotNPI <- function(dfDashboard){
  minBestDate <- min(dfDashboard$START_DATE)
  maxBestDate <- max(dfDashboard$START_DATE)
  dfLockdownCurfewRawPlotBest <- dfLockdownCurfewRaw %>%
    mutate(BoolInclude = start < maxBestDate & end > minBestDate) %>%
    filter(BoolInclude) %>%
    mutate(start = if_else(start < minBestDate, minBestDate, start),
           end = if_else(end > maxBestDate, maxBestDate, end),
           NPI = forcats::fct_reorder(NPI, start))
  plotNPI <- dfDashboard %>%
    select(START_DATE, CHU_HOSP, IPTCC.mean) %>%
    mutate(across(.cols = c(CHU_HOSP, IPTCC.mean),
                  .fns = scale)) %>%
    tidyr::pivot_longer(cols = c("CHU_HOSP", "IPTCC.mean")) %>%
    mutate(name = factor(name,
                         levels = c("CHU_HOSP", "IPTCC.mean"),
                         labels = c("Hospitalisations", "IPTCC"))) %>%
    ggplot(mapping = aes(x = START_DATE, y = value , linetype = name, group = name)) +
    geom_rect(data = dfLockdownCurfewRawPlotBest,
              mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = NPI),
              alpha = 0.5,
              inherit.aes = FALSE) +
    tidyquant::geom_ma(n = 7, color = "black") +
    theme_bw() +
    scale_fill_viridis_d(direction = -1) +
    scale_x_date(breaks = "month", date_labels = "%m-%y", date_minor_breaks = "weeks") +
    theme(legend.position = "bottom") +
    labs(x = "Date", y = "Scaled values", color = "", linetype = "")

  return(plotNPI)
}
