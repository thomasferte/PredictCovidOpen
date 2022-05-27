#' ggplotURGENCES
#'
#' @description Generate plot of emergency units data
#'
#' @param dfDashboard Data
#'
#' @return A ggplot object
#' @export
ggplotURGENCES <- function(dfDashboard){
  plotURGENCES <- dfDashboard %>%
    arrange(START_DATE) %>%
    select(START_DATE, CHU_HOSP, URG_covid_19_COUNT, URG_covid_19_PERCENT) %>%
    mutate(across(.cols = -START_DATE,
                  .fns = scale),
           Outcome7 = lead(CHU_HOSP, 7)) %>%
    tidyr::pivot_longer(cols = c(CHU_HOSP, Outcome7, URG_covid_19_COUNT, URG_covid_19_PERCENT)) %>%
    mutate(name = factor(name,
                         levels = c("CHU_HOSP", "Outcome7", "URG_covid_19_COUNT", "URG_covid_19_PERCENT"),
                         labels = c("Hospitalisations", "Hospitalisations d-7", "Urgences count", "Urgences %"))) %>%
    ggplot(mapping = aes(x = START_DATE, y = value, color = name)) +
    tidyquant::geom_ma(n = 7, linetype = 1) +
    scale_color_viridis_d(direction = -1) +
    scale_x_date(breaks = "month", date_labels = "%m-%y") +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(y = "scaled value", x = "Date", color = "")

  return(plotURGENCES)
}
