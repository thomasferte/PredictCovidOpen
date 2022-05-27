#' ggplotPCR
#'
#' @description Generate plot of PCR
#'
#' @param dfDashboard Data
#'
#' @return A ggplot object
#' @export
ggplotPCR <- function(dfDashboard){
  plotPCR <- dfDashboard %>%
    arrange(START_DATE) %>%
    select(START_DATE, CHU_HOSP, GRP_CHU_P_TOUS_AGES, GRP_GIRONDE_P_TOUS_AGES) %>%
    mutate(across(.cols = -START_DATE,
                  .fns = scale),
           Outcome7 = lead(CHU_HOSP, 7)) %>%
    tidyr::pivot_longer(cols = c(CHU_HOSP, Outcome7, GRP_CHU_P_TOUS_AGES, GRP_GIRONDE_P_TOUS_AGES)) %>%
    mutate(name = factor(name,
                         levels = c("CHU_HOSP", "Outcome7", "GRP_CHU_P_TOUS_AGES", "GRP_GIRONDE_P_TOUS_AGES"),
                         labels = c("Hospitalisations", "Hospitalisations d-7", "RT-PCR UHB", "RT-PCR Gironde"))) %>%
    ggplot(mapping = aes(x = START_DATE, y = value, color = name)) +
    tidyquant::geom_ma(n = 7, linetype = 1, size = 1) +
    scale_color_viridis_d(direction = -1) +
    scale_x_date(breaks = "month", date_labels = "%m-%y") +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(y = "Scaled value", x = "Date", color = "")

  return(plotPCR)
}
