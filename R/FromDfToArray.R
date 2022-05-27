#' FromDfToArray
#'
#' @description Transform dataframe to array
#'
#' @param df A dataframe with tume and id columns
#'
#' @return An array for Julia Frechet
#' @export
FromDfToArray <- function(df){
  dfPreArray <- df %>%
    arrange(time, id)

  ResArray <- simplify2array(by(dfPreArray %>% select(-time, -id),
                                dfPreArray$id,
                                as.matrix)) %>%
    aperm(., c(1,3,2))

  boolTestDimensions <- any(dim(ResArray) != c(length(unique(dfPreArray$time)),
                                               length(unique(dfPreArray$id)),
                                               dfPreArray %>% select(-id, -time) %>% ncol()))

  if(boolTestDimensions) stop("Problem in the array dimension")

  return(ResArray)
}
