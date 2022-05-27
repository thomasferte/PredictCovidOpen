#' OutputLme4
#'
#' @description Nice output for lme4 objects.
#'
#' @param lme4Object Lme4 object
#'
#' @return A dataframe with coef and CI
#' @export
OutputLme4 <- function(lme4Object){
  dfCoef <- lme4::fixef(lme4Object) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Features") %>%
    rename("Coef" = ".")
  dfSd <- lme4::VarCorr(lme4Object) %>% as.data.frame() %>%
    rename("Coef" = "sdcor") %>%
    mutate(Features = paste0("sd_", grp)) %>%
    select(Coef, Features)
  dfConfint <- confint(lme4Object, method = "Wald") %>%
    as.data.frame() %>%
    rename("CIlwr" = "2.5 %",
           "CIupr" = "97.5 %") %>%
    tibble::rownames_to_column(var = "Features") %>%
    left_join(dfCoef) %>%
    na.omit() %>%
    bind_rows(dfSd)
  return(dfConfint)
}
