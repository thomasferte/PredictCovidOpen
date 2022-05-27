#' COVID19 vaccin age class
#'
#'
#' The dataframe contains the following columns:
#' \itemize{
#'   \item Classe_age The corresponding age class
#'   \item indicateur_classe The class indicator
#' }
#' @format A data frame with 14 rows and 2 columns
#'
#' @docType data
#'
#' @usage data(dfClassesAgeVaccin)
#'
#' @examples
#' \dontrun{
#'
#' dfClassesAgeVaccin <- data.frame(indicateur_classe = c(0, 4, 9,
#' 11, 17, 24,
#' 29, 39, 49,
#' 59, 64, 69,
#' 74, 79, 80),
#' Classe_age = c("Tous âges", "0-4", "5-9",
#'                "10-11", "12-17", "18-24",
#'                "25-29", "30-39", "40-49",
#'                "50-59", "60-64", "65-69",
#'                "70-74", "75-79", "80 et +"))
#' save(dfClassesAgeVaccin, file = "data/dfClassesAgeVaccin.rdata")
#'
#' }
#'
"dfClassesAgeVaccin"

