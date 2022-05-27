#' dm_createVar
#'
#' @description Datamanage variables for prediction.
#'
#' @param df The dataframe of interest (obtain with datagouv_importHospitPCR)
#' @param ls_fctPastCum The functions for past cumulation (obtain with ls_fctmeanCasesDeriv)
#'
#' @return A dataframe with the newly created variables
#' @import slider
#' @import lubridate
#' @export
dm_createVar <- function(df,
                         ls_fctPastCum){
  df_result <- df %>%
    # select(-starts_with("TESTED_")) %>%
    group_by(dep) %>%
    arrange(dep, DATE) %>%
    ## cumulate past 7 days for TESTED and POSITIVE PCR variables
    mutate_at(.vars = vars(starts_with(c("P_", "TESTED_"))),
              .funs = function(x) slider::slide_dbl(x, .before = 6, .f = sum)) %>%
    ## mean value + first derivatives on the last 1, 3, 7, 14 days
    mutate_at(.vars = vars(starts_with(c("P_","TESTED_", "hosp", "FracP_"), ignore.case = FALSE)),
              .funs = ls_fctPastCum) %>%
    ## second derivatives
    mutate_at(.vars = vars(matches(match = "fct_firstDeriv1$")),
              .funs = ls_fctPastCum[grepl(names(ls_fctPastCum), pattern = "firstDeriv1$")]) %>%
    mutate_at(.vars = vars(matches(match = "fct_firstDeriv3$")),
              .funs = ls_fctPastCum[grepl(names(ls_fctPastCum), pattern = "firstDeriv3$")]) %>%
    mutate_at(.vars = vars(matches(match = "fct_firstDeriv7$")),
              .funs = ls_fctPastCum[grepl(names(ls_fctPastCum), pattern = "firstDeriv7$")]) %>%
    mutate_at(.vars = vars(matches(match = "fct_firstDeriv14")),
              .funs = ls_fctPastCum[grepl(names(ls_fctPastCum), pattern = "firstDeriv14$")]) %>%
    ungroup()

  return(df_result)
}
