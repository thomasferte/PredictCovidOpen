#' ggplotSAMU
#'
#' @description Generate SAMU plot
#'
#' @param dfDashboard data
#'
#' @return A ggplot object
#' @export
#'
#' @importFrom tidyquant geom_ma
ggplotSAMU <- function(dfDashboard = dfDashboard){
  plotSAMU <- dfDashboard %>%
    arrange(START_DATE) %>%
    select(START_DATE, CHU_HOSP, SAMU_covid_19_COUNT, SAMU_covid_19_PERCENT) %>%
    mutate(across(.cols = -START_DATE,
                  .fns = scale),
           Outcome7 = lead(CHU_HOSP, 7)) %>%
    tidyr::pivot_longer(cols = c(CHU_HOSP, Outcome7, SAMU_covid_19_COUNT, SAMU_covid_19_PERCENT)) %>%
    mutate(name = factor(name,
                         levels = c("CHU_HOSP", "Outcome7", "SAMU_covid_19_COUNT", "SAMU_covid_19_PERCENT"),
                         labels = c("Hospitalisations", "Hospitalisations d-7", "SAMU count", "SAMU %"))) %>%
    ggplot(mapping = aes(x = START_DATE, y = value, color = name)) +
    tidyquant::geom_ma(n = 7, linetype = 1) +
    scale_color_viridis_d(direction = -1) +
    scale_x_date(breaks = "month", date_labels = "%m-%y") +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(y = "scaled value", x = "Date", color = "")

  return(plotSAMU)
}
