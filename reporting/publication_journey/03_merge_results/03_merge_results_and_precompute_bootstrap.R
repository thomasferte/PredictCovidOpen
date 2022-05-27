##############################################################
##### Load Packages #####
library(PredictCovid)
library(dplyr)

##############################################################
##### Load data #####
message("-- start load data --")

lsEDSOpen <- readRDS(file = "PredictCovid/extdata/publication_datasets/lsEDSOpen.rds")
dfEDS <- lsEDSOpen$dfEDS
lsHyperparam <- readRDS(file = "PredictCovid/extdata/publication_datasets/lsHyperparamPublication.rds")

message("-- end load data --")

##############################################################
##### Create save dir #####
subDir <- paste0("PredictCovid/extdata/publication_datasets/results/", format(Sys.Date(), "%Y%m%d"))
if (!file.exists(subDir)){
  dir.create(subDir)
}

##############################################################
##### Bootstrap data #####

### best model
df_bestmodel <- lsHyperparam$dfSteps %>%
  filter(span == 21,
         model == "enet",
         df == "dfEDS",
         step == "EDSGirondeWeatherVariantsVaccine",
         rolderiv == "TRUE",
         forecast %in% c(7,14)) %>%
  select(STEP_ID)

### Pre-compute results
folder <- "PredictCovid_data/results_simu/multisteps_publication7158445/"
folder_frechet <- "PredictCovid_data/results_simu/frechet_publication7158447/"
folder_inter <- "PredictCovid_data/results_simu/multisteps_publication_interaction6925899/"
# folder <- "/home/thomas/curta/PredictCovid_data/results_simu/multisteps_publication6938860/"
# folder_frechet <- "/home/thomas/curta/PredictCovid_data/results_simu/frechet_publication6925898/"
# folder_inter <- "/home/thomas/curta/PredictCovid_data/results_simu/multisteps_publication_interaction6925899/"
# get the files paths
files_names <- list.files(folder, full.names = T) %>% unlist()
files_names_frechet <- list.files(folder_frechet, full.names = T) %>% unlist()
files_names_inter <- list.files(folder_inter, full.names = T) %>% unlist()
# load data
message("-- extract data --")
ls_resultJob <- pbapply::pbsapply(files_names,
                                  simplify = F,
                                  function(file_i){
                                    # load file
                                    lsCurta <- readRDS(file_i)
                                    # df_pred
                                    lsCurta$df_pred <- lsCurta$df_pred %>%
                                      filter(dep == "CHU_BDX") %>%
                                      mutate(PRED = if_else(PRED > 0, PRED, 0)) %>%
                                      left_join(dfEDS %>% select(START_DATE, CHU_HOSP = hosp), by = c("outcomeDate" = "START_DATE")) %>%
                                      filter(!is.na(CHU_HOSP)) %>%
                                      mutate(diff = PRED - CHU_HOSP,
                                             AE = abs(diff),
                                             RE = AE/abs(CHU_HOSP)) %>%
                                      select(slar_taskid, START_DATE, outcome, outcomeDate, PRED, STEP_ID, CHU_HOSP, diff, AE, RE)
                                    # df_varimp
                                    lsCurta$df_varimp <- df_bestmodel %>%
                                      left_join(lsCurta$df_varimp, by = "STEP_ID") %>%
                                      select(slar_taskid, STEP_ID, Features)

                                    return(lsCurta)
                                  })

df_resultJob_frechet <- pbapply::pbsapply(files_names_frechet,
                                          simplify = F,
                                          function(file_i){
                                            # load file
                                            dfPred <- readRDS(file_i)
                                            # df_pred
                                            res <- dfPred %>%
                                              filter(dep == "CHU_BDX") %>%
                                              mutate(PRED = if_else(PRED > 0, PRED, 0)) %>%
                                              left_join(dfEDS %>% select(START_DATE, CHU_HOSP = hosp), by = c("outcomeDate" = "START_DATE")) %>%
                                              filter(!is.na(CHU_HOSP)) %>%
                                              mutate(diff = PRED - CHU_HOSP,
                                                     AE = abs(diff),
                                                     RE = AE/abs(CHU_HOSP)) %>%
                                              select(slar_taskid, START_DATE, outcome, outcomeDate, PRED, STEP_ID, CHU_HOSP, diff, AE, RE)

                                            return(res)
                                          }) %>%
  bind_rows()

df_resultJob_inter <- pbapply::pbsapply(files_names_inter,
                                        simplify = F,
                                        function(file_i){
                                          # load file
                                          lsCurta <- readRDS(file_i)
                                          # df_pred
                                          res <- lsCurta$df_pred %>%
                                            filter(dep == "CHU_BDX") %>%
                                            mutate(PRED = if_else(PRED > 0, PRED, 0)) %>%
                                            left_join(dfEDS %>% select(START_DATE, CHU_HOSP = hosp), by = c("outcomeDate" = "START_DATE")) %>%
                                            filter(!is.na(CHU_HOSP)) %>%
                                            mutate(diff = PRED - CHU_HOSP,
                                                   AE = abs(diff),
                                                   RE = AE/abs(CHU_HOSP)) %>%
                                            select(slar_taskid, START_DATE, outcome, outcomeDate, PRED, STEP_ID, CHU_HOSP, diff, AE, RE)

                                          return(res)
                                        }) %>%
  bind_rows()

message("-- save data --")
## save predictions
lapply(ls_resultJob, FUN = function(x) x$df_pred) %>%
  bind_rows() %>%
  saveRDS(., file = paste0(subDir, "/dfPredBoot.rds"))

saveRDS(df_resultJob_frechet, file = paste0(subDir, "/dfFrechetPredBoot.rds"))
# saveRDS(df_resultJob_frechet, file = "extdata/publication_datasets/results/dfFrechetPredBoot.rds")
saveRDS(df_resultJob_inter, file = paste0(subDir, "/dfInterPredBoot.rds"))
# saveRDS(df_resultJob_inter, file = "extdata/publication_datasets/results/dfInterPredBoot.rds")
### save var importance
df_varimp <- lapply(ls_resultJob, FUN = function(x) x$df_varimp) %>%
  bind_rows() %>%
  saveRDS(., file = paste0(subDir, "/dfVarImpBoot.rds"))

##############################################################
##### EDS vs no EDS data #####
message("-- EDS vs no EDS start --")

dfCurtaEdsvsNoeds <- readRDS(file = "PredictCovid_data/results_simu/multisteps_publication_eds_vs_noedsNA/result_job1_20220304.rds")
# dfCurtaEdsvsNoeds <- readRDS(file = "/home/thomas/curta/PredictCovid_data/results_simu/multisteps_publication_eds_vs_noedsNA/result_job1_20220304.rds")

dfEdsvsNoeds <- dfCurtaEdsvsNoeds$df_pred %>%
  left_join(lsHyperparam$dfstepsEDSnoEDS, by = "STEP_ID") %>%
  left_join(dfEDS %>% select(START_DATE, CHU_HOSP = hosp), by = c("outcomeDate" = "START_DATE")) %>%
  select(outcome, outcomeDate, PRED, step, forecast, CHU_HOSP) %>%
  tidyr::pivot_wider(names_from = step, values_from = PRED) %>%
  tidyr::pivot_longer(cols = c("CHU_HOSP", "EDSGirondeWeatherVariantsVaccine", "HospPCRGirondeWeatherVariantVaccine"), values_to = "PRED", names_to = "source") %>%
  mutate(source = factor(source,
                         levels = c("CHU_HOSP", "HospPCRGirondeWeatherVariantVaccine", "EDSGirondeWeatherVariantsVaccine"),
                         labels = c("Observed", "No EHR", "EHR")),
         PRED = if_else(PRED < 0, 0, PRED))

dfEdsvsNoedsVarImp <- dfCurtaEdsvsNoeds$df_varimp %>%
  left_join(lsHyperparam$dfstepsEDSnoEDS, by = "STEP_ID") %>%
  filter(step == "EDSGirondeWeatherVariantsVaccine") %>%
  select(-c(slar_jobid, slar_taskid))

message("-- save data --")

saveRDS(dfEdsvsNoeds, file = paste0(subDir, "/dfEdsvsNoeds.rds"))
saveRDS(dfEdsvsNoedsVarImp, file = paste0(subDir, "/dfEdsvsNoedsVarImp.rds"))

message("-- script end --")
