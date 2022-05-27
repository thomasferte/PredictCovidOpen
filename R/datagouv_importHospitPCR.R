#' datagouv_importHospitPCR
#'
#' @description Function to import PCR and hospitalisation covid datagouv of Sante Publique France
#'
#' @param url_pcrIncDepAfterMay PCR dataset after May. Default is https://www.data.gouv.fr/en/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675
#' @param url_hospitPrevDep Hospitalisations dataset. Default is https://www.data.gouv.fr/en/datasets/r/08c18e08-6780-452d-9b8c-ae244ad529b3
#' @param url_hospitIncDep Hospitalisations incidence dataset. Default is https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c
#' @param df_chu A dataframe of one or more Hospitals. Each hospital is considered as a new dataframe Must pass quality check of dimensions and column names.
#' @param hospitIncident If TRUE, the number of incident hospitalisation over the last 7 days is retrieved instead of the prevalent hospitalisations.
#'
#' @return A dataframe with number of tested and covid positive people by age and number of people in hospitalisation and reanimation
#' @export
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' df_hospitPCR <- datagouv_importHospitPCR()
#' }
datagouv_importHospitPCR <- function(url_pcrIncDepAfterMay = "https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675",
                                     url_hospitPrevDep = "https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7",
                                     url_hospitIncDep = "https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c",
                                     df_chu = NULL,
                                     hospitIncident = FALSE){
  ##### Data management
  df_classeAge09 <- dfClassesAgeVariants %>%
    rename(cl_age90 = indicateur_classe,
           Libelle = Classe_age) %>%
    mutate(Libelle = gsub(pattern = " |-", "_", x = Libelle),
           Libelle = gsub(pattern = "__", "_", x = Libelle),
           Libelle = gsub(pattern = "\u00e2", "a", x = Libelle))

  ##### Datasets
  ### PCR
  ## After May
  df_pcrAfterMay <- read.csv(file = url_pcrIncDepAfterMay, sep = ";") %>%
    mutate(jour = as.POSIXct(jour),
           cl_age90 = factor(cl_age90, levels = df_classeAge09$cl_age90, labels = df_classeAge09[, "Libelle"])) %>%
    rename("TESTED" = "T",
           "DATE" = "jour") %>%
    tidyr::pivot_wider(id_cols = c("dep", "DATE"),
                       names_from = "cl_age90",
                       values_from = c("P", "TESTED"))


  ### Hospitalisations
  ## Hospit prevalent
  df_hospitDep <- read.csv(file = url_hospitPrevDep, sep = ";") %>%
    mutate(jour = if_else(grepl(jour, pattern = "/"),
                          as.POSIXct(jour, format = "%d/%m/%Y"),
                          as.POSIXct(jour, format = "%Y-%m-%d"))) %>%
    rename("DATE" = "jour") %>%
    filter(sexe == 0) %>%
    select(dep, DATE, hosp) %>%
    as_tibble()

  if(hospitIncident){
    ## Hospit incident
    df_hospitIncident <- read.csv(file = url_hospitIncDep, sep = ";") %>%
      mutate(jour = if_else(grepl(jour, pattern = "/"),
                            as.POSIXct(jour, format = "%d/%m/%Y"),
                            as.POSIXct(jour, format = "%Y-%m-%d"))) %>%
      rename("DATE" = "jour") %>%
      select(dep, DATE, incid_hosp) %>%
      ## sum over the last 7 days
      group_by(dep) %>%
      arrange(DATE) %>%
      mutate(hospCumWeek = as.integer(slider::slide(incid_hosp, sum, .before = 6))) %>%
      ungroup() %>%
      as_tibble() %>%
      select(-incid_hosp)

    df_hospitDep <- df_hospitIncident %>%
      full_join(df_hospitDep)
    }

  ### merge pcr and hosp
  df_pcrhosp <- df_pcrAfterMay %>%
    left_join(df_hospitDep) %>%
    mutate(FROM = "SIDEP")

  # test quality of dataframe df_pcrhosp
  message("-- test quality of df_pcrhosp --")
  fct_testqualitydf(df_to_test = df_pcrhosp, hospitIncident = hospitIncident)

  # add df_chubdx
  if(!is.null(df_chu)){
    message("-- test quality of df_chu --")
    fct_testqualitydf(df_chu, hospitIncident = hospitIncident)
    df_pcrhosp <- df_pcrhosp %>%
      bind_rows(df_chu)
  }

  ### merge both df
  df_all <- df_pcrhosp %>%
    left_join(df_dptRegion) %>%
    filter(!dep %in% c(975, 977, 978)) %>%
    # create time, day of week and hosp per 100 000 hab variables
    mutate(DATE = as.Date(DATE),
           TIME = as.numeric(difftime(DATE, min(DATE), units = "days")),
           WEEKDAY = as.factor(weekdays(DATE)),
           HOSPHAB = hosp/Population*10^5,
           dep = paste0(dep, "_", FROM)) %>%
    # add fraction of positivity variable by age category
    mutate(FracP_tous_ages = P_tous_ages/TESTED_tous_ages,
           FracP_0_9_ans = P_0_9_ans/TESTED_0_9_ans,
           FracP_10_19_ans = P_10_19_ans/TESTED_10_19_ans,
           FracP_20_29_ans = P_20_29_ans/TESTED_20_29_ans,
           FracP_30_39_ans = P_30_39_ans/TESTED_30_39_ans,
           FracP_40_49_ans = P_40_49_ans/TESTED_40_49_ans,
           FracP_50_59_ans = P_50_59_ans/TESTED_50_59_ans,
           FracP_60_69_ans = P_60_69_ans/TESTED_60_69_ans,
           FracP_70_79_ans = P_70_79_ans/TESTED_70_79_ans,
           FracP_80_89_ans = P_80_89_ans/TESTED_80_89_ans,
           FracP_90_ans_et_plus = P_90_ans_et_plus/TESTED_90_ans_et_plus) %>%
    mutate_at(.vars = vars(starts_with("FracP_")), .funs = function(x) replace(x, is.na(x), 0)) %>%
    select(reg, dep, DATE, everything()) %>%
    select(-FROM)

  return(df_all)
}





