#' PredVarimpFromFolder
#'
#' @description Function to extract prediction and variable importance from a folder.
#'
#' @param folder The folder path
#'
#' @return A list of length 2 : predictions and variable importance dataframe
#' @export
PredVarimpFromFolder <- function(folder){
  # get the files paths
  files_names <- list.files(folder, full.names = T) %>% unlist()
  # load data
  ls_resultJob <- pbapply::pbsapply(files_names,
                                    simplify = F,
                                    function(file_i){
                                      # load file
                                      lsCurta <- readRDS(file_i)

                                      return(lsCurta)
                                    })
  df_pred <- lapply(ls_resultJob, FUN = function(x) x$df_pred) %>%
    bind_rows()
  df_varimp <- lapply(ls_resultJob, FUN = function(x) x$df_varimp) %>%
    bind_rows()
  return(list(df_pred = df_pred, df_varimp = df_varimp))
}
