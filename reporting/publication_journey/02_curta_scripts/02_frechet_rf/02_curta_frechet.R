library(dplyr)
library(PredictCovid)

print(paste0("Computing start at ", Sys.time(), "..."))

########################### load data #########################
lsHyperparam <- readRDS(file = "PredictCovid/extdata/publication_datasets/lsHyperparamPublication.rds")
lsEDSOpen <- readRDS(file = "PredictCovid/extdata/publication_datasets/lsEDSOpen.rds")
lsCleanData <- list(dfEDS = lsEDSOpen$dfEDS,
                    dfOpen = lsEDSOpen$lsOpenData$dfData)
slar_taskid <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
slar_jobid <- as.numeric(Sys.getenv("SLURM_ARRAY_JOB_ID"))

# lsHyperparam <- readRDS(file = "extdata/publication_datasets/lsHyperparamPublication.rds")
# lsEDSOpen <- readRDS(file = "extdata/publication_datasets/lsEDSOpen.rds")
# lsCleanData <- list(dfEDS = lsEDSOpen$dfEDS,
#                     dfOpen = lsEDSOpen$lsOpenData$dfData)
# slar_taskid <- 50
# slar_jobid <- 1
# lsHyperparam$dfFrechet <- lsHyperparam$dfFrechet %>% filter(forecast == 7) %>% filter(Date > as.Date("2021-03-20"),
#                                                                                       Date < as.Date("2021-04-20"))

############################################################
# bootstrap ?
boot <- slar_taskid != 501
# Julia implementation ?
frechet_julia = TRUE
# cores number
numCores <- parallelly::availableCores(methods = "Slurm")
print(paste0("numCores = ", numCores))
# rows of hyperparameter dataframe
vecRow <- seq(1, nrow(lsHyperparam$dfFrechet))
# cluster function depending on rowid

ClusterFunction <- function(rowid){
  tryCatch(expr = {
    ## get hyperparam
    step <- as.character(lsHyperparam$dfFrechet[rowid, "step"])
    span <- as.numeric(lsHyperparam$dfFrechet[rowid, "span"])
    model <- lsHyperparam$dfFrechet[rowid, "model"]
    df <- as.character(lsHyperparam$dfFrechet[rowid, "df"])
    forecast <- as.numeric(lsHyperparam$dfFrechet[rowid, "forecast"])
    date <- as.Date(lsHyperparam$dfFrechet[rowid, "Date"])
    stepID <- as.numeric(lsHyperparam$dfFrechet[rowid, "STEP_ID"])

    ## compute predictions
    lsResults <- PredictionPublicationFromHyperparam(span = span,
                                                     model = model,
                                                     features = lsHyperparam$lsFeatures[[step]],
                                                     forecast = forecast,
                                                     date = date,
                                                     rolderiv = FALSE,
                                                     outcomeCol = "hosp",
                                                     df = lsCleanData[[df]],
                                                     boot = boot,
                                                     frechet_julia = frechet_julia)

    dfPred <- lsResults$dfPred %>%
      mutate(STEP_ID = stepID,
             slar_jobid = slar_jobid,
             slar_taskid = slar_taskid)

    return(dfPred)
  },
  error = function(e) e)
}

set.seed(slar_taskid)

print(paste0("Parallel computing start at ", Sys.time(), "..."))
lsResultsAll <- parallel::mclapply(X = vecRow,
                                   FUN = ClusterFunction,
                                   mc.cores = numCores)
print(paste0("Parallel computing end at ", Sys.time(), "..."))

df_pred <- bind_rows(lsResultsAll)

############################################################
##### save results
subDir <- paste0("PredictCovid_data/results_simu/frechet_publication", slar_jobid)
if (!file.exists(subDir)){
  dir.create(subDir)
}

date_str <- gsub(x = Sys.Date(),"-","")

filename_r <- paste0(subDir, "/result_job",
                     slar_taskid, "_", date_str, ".rds")

saveRDS(df_pred, file = filename_r)
