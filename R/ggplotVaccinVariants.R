#' ggplotVaccinVariants
#'
#' @description Generate plot of vaccin and variants
#'
#' @param dfDashboard The raw data
#'
#' @return a ggplot object
#' @export
#'
#' @importFrom viridis viridis
ggplotVaccinVariants <- function(dfDashboard){
  plotVaccinVariants <- dfDashboard %>%
    arrange(START_DATE) %>%
    select(any_of(c("START_DATE", "CHU_HOSP", "Pct_Variants", "n_cum_dose1_tous_ages"))) %>%
    mutate(across(.cols = -START_DATE,
                  .fns = scale),
           Outcome7 = lead(CHU_HOSP, 7)) %>%
    tidyr::pivot_longer(cols = any_of(c("CHU_HOSP", "Outcome7", "Pct_Variants", "n_cum_dose1_tous_ages"))) %>%
    mutate(name = factor(name,
                         levels = c("CHU_HOSP", "Outcome7", "Pct_Variants", "n_cum_dose1_tous_ages"),
                         labels = c("Hospitalisations", "Hospitalisations d-7", "Variants %", "Vaccination 1 dose"))) %>%
    ggplot(mapping = aes(x = START_DATE, y = value, color = name)) +
    geom_line() +
    scale_color_viridis_d(direction = -1) +
    scale_x_date(breaks = "month", date_labels = "%m-%y") +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(y = "scaled value", x = "Date", color = "")

  return(plotVaccinVariants)
}
