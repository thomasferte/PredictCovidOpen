#' CleanEDS
#'
#' @description A function to clean EDS dataframe
#'
#' @param dfEDS The EDS dataframe
#'
#' @return A clean dataframe
#' @export
CleanEDS <- function(dfEDS){
  dfEDSDepLevel <- dfEDS %>%
    rename("hosp" = "CHU_HOSP",
           "WEEKDAY" = "CHU_WEEKDAY") %>%
    rename_with(.cols = starts_with("GRP_CHU_"),
                .fn = function(x) gsub(pattern = "GRP_CHU_", replacement = "", x = x))

  ## add intereaction between all PCR features and all variants forms
  vecPCRfeatures <- grep(colnames(dfEDSDepLevel),
                         pattern = "^FRAC|^TESTED|^P_|^GRP_GIRONDE",
                         value = TRUE)
  vecVariants <- dfEDSDepLevel$Majority_variant %>% unique()
  varVaccin <- "Vaccin_1dose"
  grid_interaction <- expand.grid(variant = vecVariants, pcr = vecPCRfeatures)

  # create dfEDSInteraction with all interaction features with variant
  dfEDSInteraction <- dfEDSDepLevel
  apply(grid_interaction,
        1,
        function(row){
          pcr <- as.character(row[["pcr"]])
          variant <- as.character(row[["variant"]])

          varname <- paste("INTERVARIANT", variant, pcr, sep = "_")

          binary_variant <- as.numeric(dfEDSInteraction$Majority_variant == variant)

          dfEDSInteraction[[varname]] <<- binary_variant*dfEDSInteraction[[pcr]]

          return()
        })
  # create dfEDSInteraction with all interaction features with vaccin
  lapply(vecPCRfeatures,
         function(pcr){
           varname <- paste("INTERVACCIN", pcr, sep = "_")

           dfEDSInteraction[[varname]] <<- dfEDSInteraction[[varVaccin]]/10^6*dfEDSInteraction[[pcr]]

           return()
         })

  res <- dfEDSInteraction%>%
    janitor::remove_constant()

  return(res)
}

