#' fct_testqualitydf
#'
#' @description Function to test quality of dataframe for covid-19 prediction
#'
#' @param df_to_test A dataframe with th following features:
#' \itemize{
#'   \item 'dep' [character] - the departement (e.g 33)
#'   \item 'DATE' [POSIXct] - the date in POSIXct format
#'   \item 'P_0_9_ans', 'P_10_19_ans', ..., 'P_80_89_ans' and 'P_90_ans_et_plus' [integer] - number of PCR positive patients by age class.
#'   \item 'P_tous_ages' [integer] - total number of PCR positive patients.
#'   \item 'TESTED_0_9_ans', 'TESTED_10_19_ans', ..., 'TESTED_80_89_ans' and 'TESTED_90_ans_et_plus' [integer] - number of tested patients by age class.
#'   \item 'TESTED_tous_ages' [integer] - total number of tested patients.
#'   \item 'hosp' [integer] - total number of hospitalised patients (current hospitalisations).
#'   \item 'FROM' [character] - A character string to denote from where the data come from (e.g SIDEP, CHU, ...)
#' }
#' @param hospitIncident A boolean, TRUE if the dataframe is about incidence cases (default is FALSE).
#'
#' @return An error message if something is wrong.
#' @export
fct_testqualitydf <- function(df_to_test, hospitIncident = FALSE){
  df_type <- data.frame(feature = c('dep', 'DATE', 'P_0_9_ans', 'P_10_19_ans', 'P_20_29_ans', 'P_30_39_ans', 'P_40_49_ans', 'P_50_59_ans', 'P_60_69_ans', 'P_70_79_ans', 'P_80_89_ans', 'P_90_ans_et_plus', 'P_tous_ages', 'TESTED_0_9_ans', 'TESTED_10_19_ans', 'TESTED_20_29_ans', 'TESTED_30_39_ans', 'TESTED_40_49_ans', 'TESTED_50_59_ans', 'TESTED_60_69_ans', 'TESTED_70_79_ans', 'TESTED_80_89_ans', 'TESTED_90_ans_et_plus', 'TESTED_tous_ages', 'hosp', 'FROM'),
                        class = c('character', 'POSIXct', 'integer', 'integer', 'integer', 'integer', 'integer', 'integer', 'integer', 'integer', 'integer', 'integer', 'integer', 'integer', 'integer', 'integer', 'integer', 'integer', 'integer', 'integer', 'integer', 'integer', 'integer', 'integer', 'integer', 'character'))

  if(hospitIncident){
    df_type <- bind_rows(df_type,
                         data.frame(feature = c('hospCumWeek'),
                                    class = c('integer')))
  }

  ## 1) columns
  message("-- Test columns --")
  toomany_columns <- colnames(df_to_test)[!colnames(df_to_test) %in% df_type$feature]
  if(length(toomany_columns) != 0) stop(paste0("Too many columns: please remove ", paste0(toomany_columns, collapse = ", ")))
  missing_columns <- df_type$feature[!df_type$feature %in% colnames(df_to_test)]
  if(length(missing_columns) != 0) stop(paste0("Missing columns: please add ", paste0(missing_columns, collapse = ", ")))

  ## 2) types
  message("-- Test columns types --")
  vec_checktype <- sapply(df_to_test, function(x) class(x)[1]) %>%
    as.data.frame() %>%
    rename("class_added" = ".") %>%
    tibble::rownames_to_column("feature") %>%
    left_join(df_type) %>%
    mutate(label = paste0(feature, " (", class, ")")) %>%
    filter(class_added != class) %>%
    pull(label)

  if(length(vec_checktype) != 0) stop(paste0("Type errors: please modify ", paste0(vec_checktype, collapse = ", ")))

  message("All right buddy, your dataset is amazing \U0001f600 ")

  return()
}
