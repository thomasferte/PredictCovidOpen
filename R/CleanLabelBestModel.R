#' CleanLabelBestModel
#'
#' @description Take a model id as dataframe and change it to a nice character vector
#'
#' @param model A model ID in dataframe format
#'
#' @return A character string of model
#' @export
CleanLabelBestModel <- function(model){
  LabelbestModelCompromise <- model %>%
    transmute(Label = paste0(OUTCOME, "_",
                             SELECTION, "_span",
                             SPAN, "_",
                             Model)) %>%
    pull(Label)

  return(LabelbestModelCompromise)
}
