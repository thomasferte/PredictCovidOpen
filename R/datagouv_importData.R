#' datagouv_importData
#'
#' @description Function to import covid datagouv of Santé Publique France (PCR before and after May, Hospitalisations) and opencovid.fr
#'
#' @param url_classeAge09 Age class labels for 0-9. Default is https://www.data.gouv.fr/en/datasets/r/2acb510e-1619-4d5e-8e33-6abd5b4be455
#' @param url_classeAgeABC Age class labels for ABC. Default is https://www.data.gouv.fr/en/datasets/r/db378f2a-83a1-40fd-a16c-06c4c8c3535d
#' @param url_labelRegion Region labels. Default is https://www.data.gouv.fr/en/datasets/r/0f5643d8-026d-4532-83c6-96e68d429f56
#' @param url_pcrIncDepAfterMay PCR dataset after May. Default is https://www.data.gouv.fr/en/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675
#' @param url_pcrIncDepBeforeMay PCR dataset before May. Default is https://www.data.gouv.fr/en/datasets/r/b4ea7b4b-b7d1-4885-a099-71852291ff20
#' @param url_hospitPrevDep Hospitalisations dataset. Default is https://www.data.gouv.fr/en/datasets/r/08c18e08-6780-452d-9b8c-ae244ad529b3
#' @param url_opencovid19frChiffrescles Opencovid dataset. Default is https://raw.githubusercontent.com/opencovid19-fr/data/master/dist/chiffres-cles.csv
#' @param clean If clean is equal to true then region column (reg) is added to each datasets. Dep 975, 977, 978 are excluded from the PCR after May dataset. Only reliable ressources are taken from opencovid. Default is TRUE.
#' @param vec_reliableSources Vector of reliable ressources from opencovid. Only used if clean is T. Default is c("agences-regionales-sante", "sante-publique-france", "ministere-sante", "sante-publique-france-data", "opencovid19-fr")
#'
#' @return A list of four datasets: PCR after and before may, hospitalisations and opencovid.
#' @export
#' @import dplyr
datagouv_importData <- function(url_classeAge09 = "https://bit.ly/30YjDOW",
                                url_classeAgeABC = "https://bit.ly/2NwNGKq",
                                url_labelRegion = "https://bit.ly/3cMbrH7",
                                url_pcrIncDepAfterMay = "https://bit.ly/3eXAt90",
                                url_pcrIncDepBeforeMay = "https://bit.ly/3ltLnV8",
                                url_hospitPrevDep = "https://bit.ly/3eT8pU4",
                                url_opencovid19frChiffrescles = "https://bit.ly/30VbjPU",
                                clean = T,
                                vec_reliableSources = c("agences-regionales-sante", "sante-publique-france", "ministere-sante", "sante-publique-france-data", "opencovid19-fr")){

  ##### Data management
  df_classeAge09 <- read.csv(file = url_classeAge09, sep = ";", encoding = "UTF-8") %>%
    rename("cl_age90" = "X.U.FEFF.Identifiant")

  df_classeAgeABC <- read.csv(file = url_classeAgeABC, sep = ";", encoding = "UTF-8") %>%
    rename("clage_covid" = "X.U.FEFF.Code.tranches.d.age",
           "Label" = "X")

  df_labelRegion <- read.csv(file = url_labelRegion, sep = ";", encoding = "UTF-8") %>%
    rename("reg" = "X.U.FEFF.Code")


  ##### Datasets
  ### PCR
  ## Après Mai
  df_pcrAfterMay <- read.csv(file = url_pcrIncDepAfterMay, sep = ";") %>%
    mutate(jour = as.POSIXct(jour),
           cl_age90 = factor(cl_age90, levels = df_classeAge09$cl_age90, labels = df_classeAge09[, "Libell\u00e9"])) %>%
    rename("TESTED" = "T",
           "DATE" = "jour")

  ## Avant Mai
  df_pcrBeforeMay <- read.csv(file = url_pcrIncDepBeforeMay, sep = ";") %>%
    mutate(jour = as.POSIXct(jour),
           clage_covid = factor(clage_covid, levels = df_classeAgeABC$clage_covid, df_classeAgeABC$Label)) %>%
    rename("TESTED" = "nb_test",
           "P" = "nb_pos",
           "DATE" = "jour")

  ### Hospitalisations
  df_hospit <- read.csv(file = url_hospitPrevDep, sep = ";") %>%
    mutate(jour = if_else(grepl(jour, pattern = "/"),
                          as.POSIXct(jour, format = "%d/%m/%Y"),
                          as.POSIXct(jour, format = "%Y-%m-%d")),
           cl_age90 = factor(cl_age90, levels = df_classeAge09$cl_age90, labels = df_classeAge09[, "Libell\u00e9"])) %>%
    rename("DATE" = "jour")

  ### Données open covid sur hospit + pcr
  df_opencovid <- read.csv(file = url_opencovid19frChiffrescles, sep = ",", encoding = "UTF-8") %>%
    mutate(date = as.POSIXct(date)) %>%
    rename("DATE" = "date")

  ##### add modifications if clean is TRUE
  if(clean){
    ### add region and filter COM from after may PCR
    df_pcrAfterMay <- df_pcrAfterMay %>%
      left_join(df_dptRegion) %>%
      select(reg, everything()) %>%
      # on exclut les collectivité d'outre mer
      filter(!dep %in% c(975, 977, 978))

    ### add region to PCR before may
    df_pcrBeforeMay <- df_pcrBeforeMay %>%
      left_join(df_dptRegion) %>%
      select(reg, everything())

    ### add region to Hospitalisation
    df_hospit <- df_hospit %>%
      left_join(df_dptRegion) %>%
      select(reg, dep, everything())

    ### Filter opencovid on reliable ressources and take only departement informations, add region
    df_opencovid <- df_opencovid %>%
      filter(source_type %in% vec_reliableSources,
             granularite == "departement") %>%
      mutate(dep = gsub("DEP-", "", x = maille_code)) %>%
      left_join(df_dptRegion) %>%
      select(reg, dep, DATE, cas_confirmes, depistes) %>%
      arrange(reg, dep, DATE) %>%
      group_by(reg, dep, DATE) %>%
      summarise_all(.funs = function(x) mean(x, na.rm = T))
  }

  ### aggregate into list
  result <- list(df_pcrAfterMay = df_pcrAfterMay,
                 df_pcrBeforeMay = df_pcrBeforeMay,
                 df_hospit = df_hospit,
                 df_opencovid = df_opencovid)

  return(result)
}





