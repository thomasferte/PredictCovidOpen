#' preplot_sumOverFrRegDep
#'
#' @description Compute a preplot dataframe summing a value over a Country, region, departement by date.
#'
#' @param df Dataframe
#' @param char_colDate The date column date
#' @param vec_colMeasure The vector of measure of interest column names
#' @param char_region The region column name
#' @param char_dep The departement column name
#'
#' @return A dataframe with measure aggregated at Country, region and departement by date and by departement.
#' @export
#' @import forcats
#' @import dplyr
preplot_sumOverFrRegDep <- function(df,
                                    char_colDate,
                                    vec_colMeasure,
                                    char_region = "reg",
                                    char_dep = "dep"){

  # test if underscore in colnames and stop if so
  if(any(grepl(c(char_colDate, vec_colMeasure, char_region, char_dep), pattern = "_"))) stop("Colnames should not contain _ for the function to work")

  df_plot <- df %>%
    select(char_colDate, vec_colMeasure, char_region, char_dep) %>%
    group_by_at(.vars = char_colDate) %>%
    mutate_at(.vars = vec_colMeasure, .funs = list(FR = sum)) %>%
    group_by_at(.vars = c(char_colDate, char_region)) %>%
    mutate_at(.vars = vec_colMeasure, .funs = list(REG = sum)) %>%
    group_by_at(.vars = c(char_colDate, char_region, char_dep)) %>%
    mutate_at(.vars = vec_colMeasure, .funs = list(DEP = sum)) %>%
    ungroup() %>%
    select(-vec_colMeasure) %>%
    tidyr::gather(TYPE, MEASURE, -c(char_colDate, char_dep, char_region)) %>%
    mutate(INDICATOR = sub("_.*", "", TYPE),
           LOCALISATION = sub(".*_", "", TYPE),
           LOCALISATION = forcats::fct_relevel(LOCALISATION, "DEP", "REG", "FR"))
  return(df_plot)

}
