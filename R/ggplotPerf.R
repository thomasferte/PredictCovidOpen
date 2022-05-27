#' ggplotPerf
#'
#' @description Plot performance
#'
#' @param dfPerf The performance dataframe from Dfperformances().
#' @param type Should either "Global", "Simplified" or "SimplifiedNoSpan".
#'
#' @return A ggplot object
#' @export
#'
#' @importFrom ggsci scale_color_d3
ggplotPerf <- function(dfPerf, type = "Global"){

  if(type == "Global"){
    floorMinPerf <- floor(min(dfPerf$MRE)*10)/10
    ceilMaxPerf <- ceiling(max(dfPerf$MRE)*10)/10
    vecBreaksfigPerfGraph <- seq(from = floorMinPerf,
                                 to = ceilMaxPerf,
                                 by = 0.1)
    res <- dfPerf %>%
      mutate(SPAN = as.factor(SPAN)) %>%
      ggplot(mapping = aes(y = SELECTION, x = MRE, color = SPAN, shape = Model)) +
      geom_point(alpha = 0.8) +
      facet_grid(FORECAST ~ OUTCOME) +
      scale_color_viridis_d(end = 0.9) +
      scale_shape_discrete(labels = c("Elastic-net", "random forest")) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "bottom") +
      scale_x_log10(breaks = vecBreaksfigPerfGraph,
                    limits = c(min(dfPerf$MRE), ceilMaxPerf))
  } else if(type == "Simplified"){
    res <- dfPerf %>%
      filter(OUTCOME == "Prevalent",
             FORECAST %in% c(7,14),
             SELECTION %in% c(dfPerf %>% pull(SELECTION) %>% unique() %>% grep(pattern = "_add_wvv", value = T),
                              "SIDEP",
                              "lm_SIDEP_Model")) %>%
      mutate(SELECTION = factor(SELECTION,
                                levels = c("All_add_wvv",
                                           "VI_add_wvv",
                                           "VI_7dMaxDeriv_add_wvv",
                                           "VI_small_add_wvv",

                                           "One_step_add_wvv",
                                           "Adaptative_add_wvv",
                                           "Adaptative_3months_add_wvv",

                                           "SIDEP",
                                           "lm_SIDEP_Model"),
                                labels = c("All features",
                                           "Expert 1",
                                           "Expert 2",
                                           "Expert 3",

                                           "Selection before 2020-12-10",
                                           "Selection on train set",
                                           "Selection on train set 3 months",

                                           "PCR + Hosp SIDEP + Hosp UHB",
                                           "UHB as department in SIDEP model*")),
             SELECTION = forcats::fct_rev(SELECTION)) %>%
      mutate(SPAN = as.factor(SPAN)) %>%
      ggplot(mapping = aes(y = SELECTION, x = MRE, color = SPAN, shape = Model)) +
      geom_point(alpha = 0.8) +
      facet_grid(FORECAST ~ OUTCOME) +
      scale_color_viridis_d(end = 0.9) +
      scale_shape_discrete(labels = c("Elastic-net", "random forest")) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "bottom",
            legend.box = "vertical") +
      scale_x_log10(breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
                    limits = c(0.1, 0.6))
  } else if(type == "SimplifiedNoSpan"){
    res <- dfPerf %>%
      filter(OUTCOME == "Prevalent",
             FORECAST %in% c(7,14),
             SELECTION %in% c(dfPerf %>% pull(SELECTION) %>% unique() %>% grep(pattern = "_add_wvv", value = T),
                              "SIDEP",
                              "lm_SIDEP_Model")) %>%
      filter(SPAN == 21|SELECTION == "lm_SIDEP_Model") %>%
      mutate(SELECTION = factor(SELECTION,
                                levels = c("All_add_wvv",
                                           "VI_add_wvv",
                                           "VI_7dMaxDeriv_add_wvv",
                                           "VI_small_add_wvv",

                                           "One_step_add_wvv",
                                           "Adaptative_add_wvv",
                                           "Adaptative_3months_add_wvv",

                                           "SIDEP",
                                           "lm_SIDEP_Model"),
                                labels = c("All features",
                                           "Expert 1",
                                           "Expert 2",
                                           "Expert 3",

                                           "Selection before 2020-12-10",
                                           "Selection on train set",
                                           "Selection on train set 3 months",

                                           "PCR + Hosp SIDEP + Hosp UHB",
                                           "UHB as department in SIDEP model*")),
             SELECTION = forcats::fct_rev(SELECTION)) %>%
      mutate(SPAN = as.factor(SPAN)) %>%
      ggplot(mapping = aes(y = SELECTION, x = MRE, color = Model, shape = Model)) +
      geom_point(alpha = 0.8) +
      facet_grid(FORECAST ~ OUTCOME) +
      ggsci::scale_color_d3(labels = c("Elastic-net", "random forest"))+
      scale_shape_discrete(labels = c("Elastic-net", "random forest")) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "bottom",
            legend.box = "vertical") +
      scale_x_log10(breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
                    limits = c(0.1, 0.6))
  } else {
    stop("Argument type should be either : Global, Simplified or SimplifiedNoSpan")
  }

  return(res)
}
