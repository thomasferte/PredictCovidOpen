#' datagouvJeuxDeDonnees
#'
#' @description Import dataset of covid data available on datagouv.fr
#'
#' @param url_listesJeuxDeDonnees default is https://www.data.gouv.fr/fr/organizations/sante-publique-france/datasets.csv
#'
#' @return A dataframe with informations of different tables available. More informations at https://www.data.gouv.fr/fr/organizations/sante-publique-france/
#' @export
#' @importFrom utils read.csv
#'
#' @examples
#' \dontrun{
#' df_jeuxDeDonnees <- datagouvJeuxDeDonnees()
#' }
datagouvJeuxDeDonnees <- function(url_listesJeuxDeDonnees = "https://bit.ly/3sfCOj2"){
  df_listesJeuxDeDonnees <- read.csv(file = url_listesJeuxDeDonnees,
                                     sep = ";",
                                     encoding = "UTF-8")
  return(df_listesJeuxDeDonnees)
}
