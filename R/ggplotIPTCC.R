#' ggplotIPTCC
#'
#' @description Generate IPTCC plot
#'
#' @param dfDashboard Data
#'
#' @return A ggplot object
#' @export
ggplotIPTCC <- function(dfDashboard){
  plotIPTCC <- dfDashboard %>%
    arrange(START_DATE) %>%
    select(START_DATE, CHU_HOSP, IPTCC.mean) %>%
    mutate(across(.cols = -START_DATE,
                  .fns = scale),
           Outcome7 = lead(CHU_HOSP, 7)) %>%
    tidyr::pivot_longer(cols = c(CHU_HOSP, Outcome7, IPTCC.mean)) %>%
    mutate(name = factor(name,
                         levels = c("CHU_HOSP", "Outcome7", "IPTCC.mean"),
                         labels = c("Hospitalisations", "Hospitalisations d-7", "IPTCC"))) %>%
    ggplot(mapping = aes(x = START_DATE, y = value, color = name)) +
    tidyquant::geom_ma(n = 7, linetype = 1) +
    scale_color_viridis_d(direction = -1) +
    scale_x_date(breaks = "month", date_labels = "%m-%y") +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(y = "scaled value", x = "Date", color = "")

  return(plotIPTCC)
}
