#' ggplotBestvsOthers
#'
#' @description Plot model predictions depending on NPI
#'
#' @param df Prediction dataframe
#' @param dfNPI NPI dataframe
#'
#' @return A ggplot object
#' @export
ggplotBestvsOthers <- function(df, dfNPI){
  res <- df %>%
    ggplot(mapping = aes(x = outcomeDate, y = Error, color = name)) +
    geom_rect(data = dfNPI,
              mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = NPI),
              alpha = 0.3,
              inherit.aes = FALSE) +
    geom_line() +
    geom_line(mapping = aes(y = IPTCC.mean, linetype = "IPTCC"), color = "black") +
    geom_line(mapping = aes(y = outcome, linetype = "Hospitalisations d+7"), color = "black") +
    scale_linetype_manual(values = c(1, 3)) +
    theme_bw() +
    theme(legend.position = "bottom", legend.box="vertical") +
    labs(x = "Date", y = "Hospitalisations/IPTCC/absolute error",
         fill = "NPI",
         color = "Absolute error",
         linetype = "")
  return(res)
}
