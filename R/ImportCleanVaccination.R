#' ImportCleanVaccination
#'
#' @description Import vaccin data by department and datamange it
#'
#' @param urlVaccin the vaccin data url
#'
#' @return A data frame with departement, date and vaccin cumulative doses by age class
#' @importFrom janitor clean_names
#' @export
ImportCleanVaccination <- function(urlVaccin = "https://www.data.gouv.fr/en/datasets/r/83cbbdb9-23cb-455e-8231-69fc25d58111"){
  dfVaccination <- read.csv(urlVaccin, sep = ";") %>%
    filter(!dep %in% c("00", "975", "977", "978")) %>%
    select(dep, jour, clage_vacsi, n_cum_complet, n_cum_dose1) %>%
    mutate(clage_vacsi = factor(clage_vacsi,
                                levels = dfClassesAgeVaccin$indicateur_classe,
                                labels = dfClassesAgeVaccin$Classe_age),
           jour = as.Date(jour, "%Y-%m-%d")) %>%
    tidyr::pivot_longer(cols = c("n_cum_complet", "n_cum_dose1")) %>%
    mutate(name = paste0(name, "_", clage_vacsi)) %>%
    select(-clage_vacsi) %>%
    tidyr::pivot_wider() %>%
    janitor::clean_names()
  return(dfVaccination)
}
