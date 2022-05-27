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

############################################################
# cores number
numCores <- parallelly::availableCores(methods = "Slurm") - 5
print(paste0("numCores = ", numCores))
# rows of hyperparameter dataframe
vecRow <- seq(1, nrow(lsHyperparam$dfSteps))
# cluster function depending on rowid
ClusterFunction <- function(rowid){
  tryCatch(expr = {
    ## get hyperparam
    step <- as.character(lsHyperparam$dfSteps[rowid, "step"])
    span <- as.numeric(lsHyperparam$dfSteps[rowid, "span"])
    model <- lsHyperparam$dfSteps[rowid, "model"]
    df <- as.character(lsHyperparam$dfSteps[rowid, "df"])
    forecast <- as.numeric(lsHyperparam$dfSteps[rowid, "forecast"])
    rolderiv <- lsHyperparam$dfSteps[rowid, "rolderiv"]
    date <- as.Date(lsHyperparam$dfSteps[rowid, "Date"])
    stepID <- as.numeric(lsHyperparam$dfSteps[rowid, "STEP_ID"])

    ## compute predictions
    lsResults <- PredictionPublicationFromHyperparam(span = span,
                                                     model = model,
                                                     features = lsHyperparam$lsFeatures[[step]],
                                                     forecast = forecast,
                                                     date = date,
                                                     rolderiv = rolderiv,
                                                     outcomeCol = "hosp",
                                                     df = lsCleanData[[df]],
                                                     boot = TRUE)

    lsResults$dfPred <- lsResults$dfPred %>%
      mutate(STEP_ID = stepID,
             slar_jobid = slar_jobid,
             slar_taskid = slar_taskid)
    lsResults$dfImportance <- lsResults$dfImportance %>%
      filter(Importance != 0) %>%
      mutate(STEP_ID = stepID,
             slar_jobid = slar_jobid,
             slar_taskid = slar_taskid)
    return(lsResults)
  },
  error = function(e) e)
}

set.seed(slar_taskid)

print(paste0("Parallel computing start at ", Sys.time(), "..."))
lsResultsAll <- parallel::mclapply(X = vecRow,
                                   FUN = ClusterFunction,
                                   mc.cores = numCores)
print(paste0("Parallel computing end at ", Sys.time(), "..."))

df_pred <- lapply(lsResultsAll, FUN = function(x) x$dfPred) %>% bind_rows()
df_varimp <- lapply(lsResultsAll, FUN = function(x) x$dfImportance) %>% bind_rows()

lsResultsMerged <- list(df_pred = df_pred,
                        df_varimp = df_varimp)

##### save results
print(paste0("Saving start at ", Sys.time(), "..."))

subDir <- paste0("PredictCovid_data/results_simu/multisteps_publication", slar_jobid)
if (!file.exists(subDir)){
  dir.create(subDir)
}

date_str <- gsub(x = Sys.Date(),"-","")

filename_r <- paste0(subDir, "/result_job",
                     slar_taskid, "_", date_str, ".rds")

saveRDS(lsResultsMerged, file = filename_r)

print(paste0("Saving end at ", Sys.time(), "..."))
