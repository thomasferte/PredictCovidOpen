library(dplyr)
library(PredictCovid)

set.seed(1)

###############################################
######### load data ###########################
###############################################

dfEDS <- readRDS(file = "extdata/test_deploy/dfEDSobfuscated.rds")

###############################################
######### define hyperparameter ###############
###############################################

### features

# vecFeatures <- colnames(lsCleanData$dfEDS)[!grepl(x = colnames(lsCleanData$dfEDS), pattern = "^INTER|Vaccin_1dose|Majority_variant")]
vecFeatures <- colnames(dfEDS)[!grepl(x = colnames(dfEDS), pattern = "^INTER")]
# vecFeatures <- colnames(lsCleanData$dfEDS)[!grepl(x = colnames(lsCleanData$dfEDS), pattern = "^INTERVACCIN")]
# vecFeatures <- colnames(lsCleanData$dfEDS)[!grepl(x = colnames(lsCleanData$dfEDS), pattern = "^INTERVARIANT")]

### Date
forecastDate <- max(dfEDS$START_DATE)

### model hyperparam
lsModel <- list(vecFeatures = vecFeatures,
                forecastDate = forecastDate,
                span = 21,
                model = 'enet',
                rolderiv = TRUE,
                forecast = c(1:14),
                df = "dfEDS")

###############################################
######### train model #########################
###############################################
message("Train model")
df_pred <- lapply(lsModel$forecast,
                  FUN = function(forecast){
                    print(forecast)
                    res <- PredictCovid::PredictionPublicationFromHyperparam(span = lsModel$span,
                                                                             model = lsModel$model,
                                                                             features = lsModel$vecFeatures,
                                                                             forecast = forecast,
                                                                             date = lsModel$forecastDate,
                                                                             rolderiv = lsModel$rolderiv,
                                                                             outcomeCol = "hosp",
                                                                             df = dfEDS,
                                                                             boot = FALSE)
                    return(res$dfPred)
                  }) %>%
  dplyr::bind_rows() %>%
  mutate(forecastDist = as.numeric(outcomeDate-START_DATE),
         margin = forecastDist/7*0.1,
         PI_INF = PRED*(1-margin),
         PI_SUP = PRED*(1+margin)) %>%
  select(DATE = outcomeDate,
         PREDICTION = PRED,
         PI_INF,
         PI_SUP)

# saveRDS(df_pred, file = "extdata/test_deploy/df_pred_ref.rds")

###############################################
######### Compare with saved predictions ######
###############################################
dfpred_ref <- readRDS(file = "extdata/test_deploy/df_pred_ref.rds")

bool = any(dfpred_ref != df_pred)
ifelse(bool,
       yes = "Not equal, there is a problem",
       no = "It's working !")
