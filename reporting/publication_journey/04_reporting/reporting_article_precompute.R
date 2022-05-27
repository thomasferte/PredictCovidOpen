############################# load packages ############################
library(dplyr)
library(ggplot2)
library(PredictCovid)

############################ load data ###################################
lsEDSOpen <- readRDS(file = "extdata/publication_datasets/lsEDSOpen.rds")
dfDashboardRaw <- lsEDSOpen$dfEDS %>% rename(CHU_HOSP = hosp)
lsOpenData <- lsEDSOpen$lsOpenData
lsHyperparam <- readRDS(file = "extdata/publication_datasets/lsHyperparamPublication.rds")

### load results
folder_results_path <- "extdata/publication_datasets/results/20220311//"
dfEDSvsNoEDS <- readRDS(file = paste0(folder_results_path, "dfEdsvsNoeds.rds"))
dfEdsvsNoedsVarImp <- readRDS(file = paste0(folder_results_path, "dfEdsvsNoedsVarImp.rds"))
dfFrechetPredBoot <- readRDS(file = paste0(folder_results_path, "dfFrechetPredBoot.rds")) %>%
  filter(slar_taskid <= 500)
dfInter <- readRDS(file = paste0(folder_results_path, "dfInterPredBoot.rds"))
dfPredBoot <- readRDS(file = paste0(folder_results_path, "dfPredBoot.rds"))
dfVarImpBoot <- readRDS(file = paste0(folder_results_path, "dfVarImpBoot.rds"))
dfVariants <- readRDS(file = "extdata/publication_datasets/dfVariants.rds")

# dfPred <- readRDS(file = "extdata/publication_datasets/results/dfPredArticle.rds")
# dfVarimp <- readRDS(file = "extdata/publication_datasets/results/dfVarimpArticle.rds")
# dfPredFrechet <- readRDS(file = "extdata/publication_datasets/results/Frechet_dfPred.rds")

############################## pre-computation #########################

###### Results - descriptive graph #####
# ### Import majority variant
# dfVariants <- ImportMajorityVariant() %>%
#   mutate(Majority_variant = as.character(Majority_variant))
# saveRDS(object = dfVariants, file = "extdata/publication_datasets/dfVariants.rds")
variantFigure <- dfVariants %>%
  mutate(Majority_variant = factor(Majority_variant,
                                   levels = c("wild",
                                              "Alpha",
                                              "Beta_or_Gamma",
                                              "E484K",
                                              "E484Q",
                                              "L452R",
                                              "Omicron",
                                              "NOID"),
                                   labels = c("Wild",
                                              "20I/501Y.V1",
                                              "20H/501Y.V2 or 20J/501Y.V3",
                                              "E484K",
                                              "E484Q",
                                              "L452R",
                                              "Omicron",
                                              "Not identifiable")
  )) %>%
  ggplot(mapping = aes(x = DATE, y = dep, color = Majority_variant)) +
  geom_point() +
  scale_color_brewer(palette = "Set2") +
  scale_x_date(breaks = "months", date_labels = "%m/%y") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 6)) +
  labs(y = "Department",
       x = "Date",
       color = "Majority variant")

PCR <- ggplotPCR(dfDashboard = dfDashboardRaw %>%
                   select(START_DATE,
                          CHU_HOSP,
                          GRP_CHU_P_TOUS_AGES = P_TOUS_AGES,
                          GRP_GIRONDE_P_TOUS_AGES)) +
  geom_vline(xintercept = as.Date("2021-04-01"), linetype = 2, size = 1) +
  geom_vline(xintercept = as.Date("2020-12-01"), linetype = 2, size = 1) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position = "bottom")

cumNewHosp <- dfDashboardRaw %>%
  select(START_DATE, IN_HOSP_in_COUNT) %>%
  pull(IN_HOSP_in_COUNT) %>%
  sum()

dateStudy <- c(min(dfDashboardRaw$START_DATE), max(dfDashboardRaw$START_DATE))

dfDiagBeofreLOCF <- lsOpenData$lsDiagImputation$dfDiagBeofreLOCF %>%
  select(-dep) %>%
  summarise_all(max)

################### Results - Analysis of the best model ######

cleanFeatures <- function(text_to_clean){
  text_to_clean %>%
    gsub(pattern = "PCR_INTRA_EXTRA_prelevements_intra_hospitaliers_COUNT", replacement = "CHU positive PCR") %>%
    gsub(pattern = "AH.mean", replacement = "Humidity") %>%
    gsub(pattern = "dewpoint.mean", replacement = "Dew point") %>%
    gsub(pattern = "FRACP_", replacement = "% positive RT-PCR ") %>%
    gsub(pattern = "GRP_GIRONDE_", replacement = "Gironde ") %>%
    gsub(pattern = "hosp", replacement = "Hospitalisation") %>%
    gsub(pattern = "IN_HOSP_in_COUNT", replacement = "New hospitalisation") %>%
    gsub(pattern = "IN_ICU_in", replacement = "New ICU") %>%
    gsub(pattern = "TDM_tdm", replacement = "CT scan") %>%
    gsub(pattern = "IPTCC.mean", replacement = "IPTCC") %>%
    gsub(pattern = "rolMax", replacement = "Maximum") %>%
    gsub(pattern = "rolMean", replacement = "Mean") %>%
    gsub(pattern = "rolMin", replacement = "Minimum") %>%
    gsub(pattern = "rolDeriv", replacement = "derivative ") %>%
    gsub(pattern = "60_90_PLUS_ANS", replacement = "60+ yo") %>%
    gsub(pattern = "0_19_ANS", replacement = "0-19 yo") %>%
    gsub(pattern = "20_59_ANS", replacement = "20-59 yo") %>%
    gsub(pattern = "TOUS_AGES", replacement = "All age groups") %>%
    gsub(pattern = "TESTED_", replacement = "RT-PCR ") %>%
    gsub(pattern = "^P_", replacement = "positive RT-PCR ") %>%
    gsub(pattern = "SAMU_", replacement = "SAMU ") %>%
    gsub(pattern = "URG_PEL_", replacement = "Emergency (site 1) ") %>%
    gsub(pattern = "URG_SA_", replacement = "Emergency (site 2) ") %>%
    gsub(pattern = "URG_PED_", replacement = "Emergency (pediatric) ") %>%
    gsub(pattern = "URG_", replacement = "Emergency (all site) ") %>%
    gsub(pattern = "WEEKDAY_lundi", replacement = "Weekday Monday") %>%
    gsub(pattern = "cephalee_2", replacement = "headache") %>%
    gsub(pattern = "fievre", replacement = "fever") %>%
    gsub(pattern = "dyspnee", replacement = "dyspnea") %>%
    gsub(pattern = "_COUNT", replacement = "") %>%
    gsub(pattern = "_PERCENT", replacement = " % of sojourn") %>%
    gsub(pattern = "3$", replacement = "3 days") %>%
    gsub(pattern = "10$", replacement = "10 days") %>%
    gsub(pattern = "CHU", replacement = "UHB")
}

denom_date <- dfEdsvsNoedsVarImp %>% pull(Date) %>% unique() %>% length()
dfFigImp <- dfEdsvsNoedsVarImp %>%
  select(Features, Importance, forecast) %>%
  filter(Features != "(Intercept)",
         forecast %in% c(7, 14)) %>%
  group_by(Features, forecast) %>%
  summarise(pct_select = sum(Importance != 0)/denom_date) %>%
  group_by(forecast) %>%
  top_n(n = 20) %>%
  mutate(Features = cleanFeatures(Features))

figImportance <- lapply(rev(unique(dfFigImp$forecast)),
                        FUN = function(x){
                          labTitle <- paste0("Forecast : ", x, " days")
                          dfFigImp %>%
                            filter(forecast == x) %>%
                            mutate(Features = forcats::fct_reorder(Features, pct_select)) %>%
                            ggplot(mapping = aes(x = pct_select, y = Features)) +
                            geom_point() +
                            labs(x = "% selected",
                                 y = "",
                                 title = labTitle) +
                            theme_bw() +
                            xlim(c(0.7, 1.00))
                        }) %>%
  ggpubr::ggarrange(plotlist = ., ncol = 1)

## Plot prediction in delay style
dfPred <- dfEDSvsNoEDS %>%
  filter(source == "EHR")

plotMultipleDelay7d <- ggplotMultipleLag(vecMultipleLag = c(3, 5, 7),
                                         dfDashboardRaw = dfDashboardRaw,
                                         dfPredictions = dfPred %>%
                                           rename(FORECAST = forecast,
                                                  Prediction = PRED)) +
  scale_x_date(breaks = "month", date_labels = "%Y-%m") +
  labs(title = "Forecast 7 days") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position = "bottom")

plotMultipleDelay14d <- ggplotMultipleLag(vecMultipleLag = c(2, 4, 6, 8, 10, 12, 14),
                                          dfDashboardRaw = dfDashboardRaw,
                                          dfPredictions = dfPred %>%
                                            rename(FORECAST = forecast,
                                                   Prediction = PRED)) +
  scale_x_date(breaks = "month", date_labels = "%Y-%m") +
  labs(title = "Forecast 14 days") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position = "bottom")

figDelay <- ggpubr::ggarrange(plotMultipleDelay7d, plotMultipleDelay14d, nrow = 2)

## Plot predictions in movie style
figPredMovie <- dfPredBoot %>%
  right_join(lsHyperparam$dfSteps %>%
               filter(span == 21,
                      step == "EDSGirondeWeatherVariantsVaccine",
                      rolderiv == "TRUE",
                      model == "enet",
                      forecast <= 14)) %>%
  group_by(forecast, outcomeDate, START_DATE, outcome) %>%
  summarise(PRED_ENET = median(PRED)) %>%
  ungroup() %>%
  mutate(label_forecast = "Forecast 1-14 days") %>%
  rename(FORECAST = forecast) %>%
  ggplotBestModel(dfDashboard = dfDashboardRaw) +
  facet_grid(label_forecast ~ .) +
  scale_x_date(breaks = "month", date_labels = "%Y-%m", limits = c(min(dfPred$outcomeDate), max(dfPredBoot$outcomeDate))) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position = "right")

## EDS vs no EDS

vecLabForecast <- c("Forecast : 7 days", "Forecast : 14 days")
names(vecLabForecast) <- c(7, 14)
vecLabForecast2 <- c("7 days", "14 days")
names(vecLabForecast2) <- c(7, 14)

colorEDSmodel <- "#00AFF5"

plotEDSvsNoEDS <- dfEDSvsNoEDS %>%
  tidyr::pivot_wider(values_from = PRED, names_from = source) %>%
  na.omit() %>%
  tidyr::pivot_longer(cols = c("Observed", "EHR", "No EHR"), names_to = "source", values_to = "PRED") %>%
  mutate(source = factor(source, levels = c("Observed", "No EHR", "EHR"))) %>%
  ggplot(mapping = aes(x = outcomeDate, y = PRED, color = source)) +
  geom_line(lwd = 1) +
  facet_grid(forecast ~ ., labeller = labeller(forecast = vecLabForecast2)) +
  scale_color_manual(values = c("black", "#ECA400", colorEDSmodel)) +
  labs(x = "Date", y = "Hospitalisations", color = "Forecast (days)") +
  scale_x_date(breaks = "month", date_labels = "%Y-%m", limits = c(min(dfPred$outcomeDate), max(dfPredBoot$outcomeDate))) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position = "right",
        legend.title = element_text(colour = "#00000000"))

dfEDSvsNoEDS %>%
  filter(source == "Observed") %>%
  select(-forecast) %>%
  arrange(outcomeDate) %>%
  distinct() %>%
  pull(outcomeDate) %>% anyDuplicated()

# Results Bootstrap

ndigits <- 3

roundCI <- function(x, digits = 3){
  sprintflab <- paste0("%.", digits, "f")
  paste0(sprintf(sprintflab, round(median(x), digits)),
         " [", sprintf(sprintflab, round(quantile(x = x, prob = 0.025), digits)),
         " ; ", sprintf(sprintflab, round(quantile(x = x, prob = 0.975), digits)), "]")
}

dfFrechetPerfBoot <- dfFrechetPredBoot %>%
  left_join(lsHyperparam$dfFrechet) %>%
  select(slar_taskid, step, span, model, df, forecast, AE, RE) %>%
  group_by(slar_taskid, step, span, model, df, forecast) %>%
  summarise_at(.vars = c("AE", "RE"), .funs = list(median = median)) %>%
  group_by(step, span, model, df, forecast) %>%
  summarise(AE_median = roundCI(AE_median, digits = 2),
            RE_median = roundCI(RE_median, digits = 3)) %>%
  ungroup() %>%
  mutate(rolderiv = FALSE)

dfPerfBoot <- dfPredBoot %>%
  left_join(lsHyperparam$dfSteps) %>%
  bind_rows(dfInter %>% left_join(lsHyperparam$dfInteraction)) %>%
  select(slar_taskid, step, span, model, rolderiv, df, forecast, AE, RE) %>%
  filter(forecast %in% c(7, 11, 14, 18)) %>%
  group_by(slar_taskid, step, span, model, rolderiv, df, forecast) %>%
  summarise_at(.vars = c("AE", "RE"), .funs = list(median = median)) %>%
  group_by(step, span, model, rolderiv, df, forecast) %>%
  summarise(AE_median = roundCI(AE_median, digits = 2),
            RE_median = roundCI(RE_median, digits = 3)) %>%
  ungroup() %>%
  bind_rows(dfFrechetPerfBoot)

# MAE/MRE over time
dfCutDate <- dfPredBoot %>%
  select(outcomeDate) %>%
  distinct() %>%
  mutate(cutdate = cut.Date(outcomeDate, breaks = "1 month")) %>%
  group_by(cutdate) %>%
  mutate(minDate = min(outcomeDate),
         maxDate = max(outcomeDate),
         rangeDate = paste0("(", minDate, " ; ", maxDate, ")"),
         middleDate = median(outcomeDate))

labels_errorplot <- c("median absolute error", "median relative error")
names(labels_errorplot) <- c("AE", "RE")

errorvalue <- dfPredBoot %>%
  left_join(lsHyperparam$dfSteps) %>%
  select(slar_taskid, outcomeDate, step, span, model, rolderiv, df, forecast, AE, RE) %>%
  filter(forecast %in% c(7, 14),
         model == "enet",
         span == 21,
         df == "dfEDS",
         rolderiv == TRUE,
         step == "EDSGirondeWeatherVariantsVaccine") %>%
  left_join(dfCutDate) %>%
  group_by(slar_taskid, rangeDate, middleDate, forecast) %>%
  summarise_at(.vars = c("AE", "RE"), .funs = median) %>%
  group_by(rangeDate, middleDate, forecast) %>%
  summarise_at(c("AE", "RE"),
               list(median = median,
                    lwr = function(x) quantile(x, 0.025),
                    upr = function(x) quantile(x, 0.975))) %>%
  ungroup() %>%
  tidyr::pivot_longer(cols = AE_median:RE_upr) %>%
  tidyr::separate(name, c("Measure", "Parameter"), sep = "_") %>%
  tidyr::pivot_wider(names_from = Parameter, values_from = value) %>%
  mutate(forecast = factor(forecast,
                           levels = c(7, 14),
                           labels = c("forecast 7 days", "forecast 14 days"))) %>%
  ggplot(mapping = aes(x = middleDate,
                       y = median,
                       ymin = lwr,
                       ymax = upr,
                       color = forecast)) +
  geom_line(bind_rows(dfDashboardRaw %>%
                        mutate(CHU_HOSP = CHU_HOSP/sd(CHU_HOSP)*10,
                               Measure = "AE"),
                      dfDashboardRaw %>%
                        mutate(CHU_HOSP = CHU_HOSP/sd(CHU_HOSP)*0.3,
                               Measure = "RE")),
            mapping = aes(x = START_DATE, y = CHU_HOSP, color = "Scaled hospitalisations"),
            size = 1,
            inherit.aes = FALSE) +
  geom_pointrange(fatten = 2, position = position_dodge(width = 10), size = 1) +
  facet_wrap(. ~ Measure, scales = "free_y", labeller = labeller(Measure = labels_errorplot)) +
  scale_color_manual(values = c("#231F20", "#BB4430", "grey")) +
  scale_x_date(breaks = c(unique(dfCutDate$minDate), max(dfCutDate$maxDate)),
               minor_breaks = NULL,
               limits = c(min(dfCutDate$minDate), max(dfCutDate$maxDate)),
               date_labels = "%d-%m-%y") +
  labs(x = "Date", y = "Error value", color = "") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position = "bottom")

dfBootPlot <- lsHyperparam$dfSteps %>%
  filter(step == "EDSGirondeWeatherVariantsVaccine",
         span == 21,
         rolderiv == "TRUE",
         df == "dfEDS",
         model == "enet",
         forecast %in% c(7, 14)) %>%
  left_join(dfPredBoot) %>%
  select(START_DATE, outcome, outcomeDate, forecast, PRED) %>%
  na.omit() %>%
  group_by(START_DATE, outcome, outcomeDate, forecast) %>%
  summarise(PRED_MEDIAN = median(PRED),
            PRED_INF = quantile(PRED, 0.025),
            PRED_SUP = quantile(PRED, 0.975))

bootPI <- dfBootPlot %>%
  mutate(forecastQuali = as.factor(forecast)) %>%
  ggplot(mapping = aes(x = outcomeDate, y = PRED_MEDIAN, ymin = PRED_INF, ymax = PRED_SUP,
                       group = forecast)) +
  geom_ribbon(alpha = 0.5, fill = colorEDSmodel) +
  geom_line(color = colorEDSmodel, lwd = 1) +
  geom_line(data = dfDashboardRaw %>% filter(START_DATE >= min(dfBootPlot$outcomeDate)),
            mapping = aes(x = START_DATE, y = CHU_HOSP),
            inherit.aes = FALSE,
            color = "black",
            lwd = 1) +
  facet_grid(forecast ~ ., labeller = labeller(forecast = vecLabForecast)) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position = "bottom") +
  labs(x = "Date", y = "Hospitalisations", color = "Forecast (days)", fill = "Forecast (days)") +
  scale_x_date(breaks = "month", date_labels = "%Y-%m")

dfBootBestModel <- lsHyperparam$dfSteps %>%
  filter(step == "EDSGirondeWeatherVariantsVaccine",
         span == 21,
         rolderiv == "TRUE",
         df == "dfEDS",
         model == "enet",
         forecast %in% c(7, 14))

dfHandmadePlot <- dfBootBestModel %>%
  left_join(dfPredBoot) %>%
  select(START_DATE, outcome, outcomeDate, forecast, PRED) %>%
  na.omit() %>%
  group_by(START_DATE, outcome, outcomeDate, forecast) %>%
  summarise(PRED_MEDIAN = median(PRED)) %>%
  ungroup() %>%
  mutate(PRED_INF = PRED_MEDIAN*(1-forecast/70),
         PRED_SUP = PRED_MEDIAN*(1+forecast/70))

plotHandmadePI <- dfHandmadePlot %>%
  mutate(forecastQuali = as.factor(forecast)) %>%
  ggplot(mapping = aes(x = outcomeDate, y = PRED_MEDIAN, ymin = PRED_INF, ymax = PRED_SUP,
                       group = forecast)) +
  geom_ribbon(alpha = 0.5, fill = colorEDSmodel) +
  geom_line(color = colorEDSmodel, lwd = 1) +
  geom_line(data = dfDashboardRaw %>% filter(START_DATE >= min(dfHandmadePlot$outcomeDate)),
            mapping = aes(x = START_DATE, y = CHU_HOSP),
            inherit.aes = FALSE,
            color = "black",
            lwd = 1) +
  facet_grid(forecast ~ ., labeller = labeller(forecast = vecLabForecast)) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position = "bottom") +
  labs(x = "Date", y = "Hospitalisations", color = "Forecast (days)", fill = "Forecast (days)") +
  scale_x_date(breaks = "month", date_labels = "%Y-%m")

plotHandmadePIfillLegend <- dfHandmadePlot %>%
  mutate(forecastQuali = as.factor(forecast)) %>%
  ggplot(mapping = aes(x = outcomeDate, y = PRED_MEDIAN, ymin = PRED_INF, ymax = PRED_SUP,
                       group = forecast)) +
  geom_ribbon(alpha = 0.5, fill = colorEDSmodel) +
  geom_line(color = colorEDSmodel, lwd = 1) +
  geom_line(data = dfDashboardRaw %>% filter(START_DATE >= min(dfHandmadePlot$outcomeDate)),
            mapping = aes(x = START_DATE, y = CHU_HOSP, color = "Observed"),
            inherit.aes = FALSE,
            lwd = 1) +
  facet_grid(forecast ~ ., labeller = labeller(forecast = vecLabForecast2)) +
  theme_bw() +
  labs(x = "Date", y = "Hospitalisations", color = "Forecast (days)", fill = "") +
  scale_x_date(breaks = "month", date_labels = "%Y-%m", limits = c(min(dfPred$outcomeDate), max(dfPredBoot$outcomeDate))) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position = "right",
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.key = element_rect(fill = "white")) +
  scale_color_manual(values = "black", guide = guide_legend(override.aes = list(color = "white")))

vecBreakDates <- c(min(dfHandmadePlot$START_DATE),
                   as.Date(c("2021-01-01", "2021-03-01", "2021-05-01", "2021-07-01", "2021-09-01", "2021-11-01")),
                   max(dfHandmadePlot$START_DATE+1))

vecLabelDates <- lapply(2:length(vecBreakDates),
                        function(i){
                          paste0("[", vecBreakDates[i-1], " ; ", vecBreakDates[i], ")")
                        }) %>%
  unlist()

dfPerfCoverage <- dfHandmadePlot %>% mutate(ip = "handmade") %>%
  bind_rows(dfBootPlot %>% mutate(ip = "boot")) %>%
  mutate(bool_inboot = PRED_INF <= outcome & PRED_SUP >= outcome,
         cut_date = cut(START_DATE,
                        breaks = vecBreakDates,
                        include.lowest = TRUE,
                        labels = vecLabelDates))

## Graph Bootstrap and EHR vs no EHR

commonPIEDSvsNoEDS <- ggpubr::ggarrange(figPredMovie + ggpubr::rremove("ylab") + ggpubr::rremove("xlab") + labs(title = "A. Prediction 1-14 days"),
                                        plotHandmadePIfillLegend + ggpubr::rremove("ylab") + ggpubr::rremove("xlab") + labs(title = "B. Prediction interval"),
                                        plotEDSvsNoEDS + ggpubr::rremove("ylab") + ggpubr::rremove("xlab") + labs(title = "C. EHR vs no EHR"),
                                        nrow = 3,
                                        common.legend = FALSE)
figPIEDSvsNoEDS <- ggpubr::annotate_figure(commonPIEDSvsNoEDS,
                                           left = grid::textGrob("Hospitalizations", rot = 90, vjust = 1),
                                           bottom = grid::textGrob("Date", vjust = 0))
## Features importance

nbdays <- lsHyperparam$dfSteps$Date %>% unique() %>% length()
dfVarImpCI <- dfVarImpBoot %>%
  filter(Features != "(Intercept)") %>%
  left_join(lsHyperparam$dfSteps) %>%
  group_by(Features, forecast, slar_taskid) %>%
  summarise(pct = n()/nbdays) %>%
  group_by(Features, forecast) %>%
  summarise(median = median(pct),
            CI_inf = quantile(pct, 0.025),
            CI_sup = quantile(pct, 0.975)) %>%
  group_by(forecast) %>%
  top_n(n = 20, wt = median) %>%
  mutate(Features = cleanFeatures(Features))

figImportanceCI <- lapply(rev(unique(dfVarImpCI$forecast)),
                          FUN = function(x){
                            labTitle <- paste0("Forecast : ", x, " days")
                            dfVarImpCI %>%
                              filter(forecast == x) %>%
                              mutate(Features = forcats::fct_reorder(Features, median)) %>%
                              ggplot(mapping = aes(x = median, y = Features, xmin = CI_inf, xmax = CI_sup)) +
                              geom_point() +
                              geom_errorbar() +
                              labs(x = "% selected",
                                   y = "",
                                   title = labTitle) +
                              theme_bw() +
                              xlim(c(0.55, 1.00))
                          }) %>%
  ggpubr::ggarrange(plotlist = ., ncol = 1)

dfCutDateVarImp <- lsHyperparam$dfSteps %>%
  select(Date) %>%
  distinct() %>%
  mutate(cutDate = cut.Date(Date, breaks = "month"),
         cutDate = gsub(pattern = "-01$", replacement = "", x = cutDate)) %>%
  group_by(cutDate) %>%
  mutate(nbDays = n())

dfPlotVariationVarImpTemp <- dfVarImpBoot %>%
  filter(Features != "(Intercept)") %>%
  left_join(lsHyperparam$dfSteps) %>%
  left_join(dfCutDateVarImp) %>%
  # pct selection in each bootstrap sample
  group_by(cutDate, Features, forecast, slar_taskid) %>%
  summarise(pct = n()/nbDays) %>%
  # take the median of all bootstrap samples
  group_by(Features, cutDate, forecast) %>%
  summarise(median = median(pct)) %>%
  # keep only features reaching at least once super high selection level
  group_by(Features, forecast) %>%
  mutate(maxPct = max(median)) %>%
  ungroup() %>%
  mutate(Features = cleanFeatures(Features)) %>%
  filter(maxPct == 1) %>%
  select(-maxPct)

lsPlotVarImpEvol <- lapply(rev(unique(dfPlotVariationVarImpTemp$forecast)),
                           FUN = function(x){
                             dfPlotVariationVarImpTempForecast <- dfPlotVariationVarImpTemp %>%
                               filter(forecast == x)

                             # add missing days
                             dfFullGrid <- expand.grid(Features = unique(dfPlotVariationVarImpTempForecast$Features),
                                                       cutDate = unique(dfPlotVariationVarImpTempForecast$cutDate),
                                                       forecast = unique(dfPlotVariationVarImpTempForecast$forecast))

                             dfPlotVariationVarImp <- dfFullGrid %>%
                               left_join(dfPlotVariationVarImpTempForecast) %>%
                               mutate(median = if_else(is.na(median), 0, median))

                             ggplot(dfPlotVariationVarImp, mapping = aes(x = cutDate, y = Features, color = median)) +
                               geom_point() +
                               labs(x = "Month",
                                    y = "",
                                    color = "% selected") +
                               theme_bw() +
                               scale_color_gradient(low = "white", high = "#30638E") +
                               theme(axis.text.x=element_text(angle=60, hjust=1), legend.position = "bottom")
                           })

# explore pediatric emergency
dfPlotVariationVarImpUrgPed <- dfVarImpBoot %>%
  filter(grepl(x = Features, pattern = "^URG_PED")) %>%
  left_join(lsHyperparam$dfSteps) %>%
  filter(forecast == 14) %>%
  left_join(dfCutDateVarImp) %>%
  # pct selection in each bootstrap sample
  group_by(cutDate, Features, forecast, slar_taskid) %>%
  summarise(pct = n()/nbDays) %>%
  # take the median of all bootstrap samples
  group_by(Features, cutDate, forecast) %>%
  summarise(median = median(pct)) %>%
  # keep only features reaching at least once super high selection level
  group_by(Features, forecast) %>%
  mutate(maxPct = max(median)) %>%
  ungroup() %>%
  filter(maxPct >= 0.2) %>%
  select(-maxPct) %>%
  ungroup() %>%
  mutate(Features = cleanFeatures(Features))


PlotVariationVarImpUrgPed <- ggplot(dfPlotVariationVarImpUrgPed,
                                    mapping = aes(x = cutDate, y = Features, color = median)) +
  geom_point() +
  labs(x = "Month",
       y = "",
       color = "% selected") +
  theme_bw() +
  scale_color_gradient(low = "white", high = "#30638E") +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.position = "bottom")

# compare vaccine with interaction vs no vaccine
plotVaccinevsNovaccine <- dfPredBoot %>%
  left_join(lsHyperparam$dfSteps) %>%
  bind_rows(dfInter %>% left_join(lsHyperparam$dfInteraction)) %>%
  filter(span == 21,
         rolderiv == "TRUE",
         model == "enet",
         forecast %in% c(7, 14),
         step %in% c("EDSGirondeWeather", "EDSGirondeWeatherVariantsVaccineInterVaccine")) %>%
  group_by(START_DATE, outcome, outcomeDate, step, forecast) %>%
  summarise(median_pred = median(PRED)) %>%
  ungroup() %>%
  tidyr::pivot_wider(names_from = step, values_from = median_pred) %>%
  tidyr::pivot_longer(cols = c("outcome", "EDSGirondeWeather", "EDSGirondeWeatherVariantsVaccineInterVaccine")) %>%
  mutate(name = factor(name,
                       levels = c("outcome", "EDSGirondeWeatherVariantsVaccineInterVaccine", "EDSGirondeWeather"),
                       labels = c("Observed", "Vaccine (with interaction)", "No vaccine"))) %>%
  ggplot(mapping = aes(x = outcomeDate, y = value, color = name)) +
  geom_line(lwd = 1) +
  facet_grid(forecast ~ ., labeller = labeller(forecast = vecLabForecast)) +
  scale_color_manual(values = c("black", "#ECA400", colorEDSmodel)) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position = "bottom") +
  labs(x = "Date", y = "Hospitalisations", color = "") +
  scale_x_date(breaks = "month", date_labels = "%Y-%m")

############################################################################
########################### save ###########################################
############################################################################
save(variantFigure,
     PCR,
     cumNewHosp,
     dateStudy,
     dfDiagBeofreLOCF,
     figImportance,
     figDelay,
     figPredMovie,
     plotEDSvsNoEDS,
     errorvalue,
     bootPI,
     plotHandmadePI,
     dfPerfCoverage,
     # vecBreakDates,
     # vecLabelDates,
     figPIEDSvsNoEDS,
     lsPlotVarImpEvol,
     dfPerfBoot,
     figImportanceCI,
     plotVaccinevsNovaccine,
     PlotVariationVarImpUrgPed,
     file = paste0(folder_results_path, "/precompute_for_report.rdata"))
