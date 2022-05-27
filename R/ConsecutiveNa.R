#' ConsecutiveNa
#'
#' @description Check the max number of consecutive NA by dep among the vars_to_check.
#'
#' @param df The dataframe of interest.
#' @param vars_to_check The features to check.
#'
#' @return A dataframe with the max consecutive NA by dep and by feature.
#' @export
ConsecutiveNa <- function(df, vars_to_check){
  res <- df %>%
    group_by(dep) %>%
    arrange(dep, DATE) %>%
    summarise_at(.vars = vars_to_check,
                 .funs = function(x){
                   vecRle <- rle(is.na(x))
                   resRle <- vecRle$lengths[vecRle$value]
                   res <- ifelse(length(resRle) == 0, 0, max(resRle))
                   return(res)
                 })
  return(res)
}
