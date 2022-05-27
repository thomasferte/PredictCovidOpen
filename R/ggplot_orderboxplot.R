#' reorder_within
#'
#' @description function to order boxplot output
#'
#' @param x internal
#' @param by internal
#' @param within internal
#' @param fun internal
#' @param sep internal
#' @param ... internal
#'
#' @return internal
#' @export
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}


#' scale_y_reordered
#'
#' @description function to order boxplot output
#'
#' @param ... internal
#' @param sep internal
#'
#' @return internal
#' @export
scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}
