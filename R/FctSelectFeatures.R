#' FctSelectFeatures
#'
#' @description Function to select relevant variables as mentionned by param_X
#'
#' @param df A dataframe with all the variables
#' @param param_X A character vector either: 'hosp', 'pcr', or 'pcrhosp'
#'
#' @return Remove the selected columns from the dataframe
#' @export
FctSelectFeatures <- function(df, param_X){
  if(param_X == "hosp"){

    dfResult <- df %>%
      select(-grep(colnames(df), pattern = "^P_|^TESTED|FracP_", value = T))

  } else if(param_X == "pcr"){

    dfResult <- df %>%
      select(-grep(colnames(df), pattern = "^hosp|HOSPHAB", value = T))

  } else if(param_X == "pcrhosp"){

    dfResult <- df

  } else{
    errorCondition(message = "X must be hosp, pcr or pcrhosp")
  }
  return(dfResult)
}
