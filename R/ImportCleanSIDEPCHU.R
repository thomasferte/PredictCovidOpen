#' ImportCleanSIDEPCHU
#'
#' @description Import SIDEP data, aggregate with CHU and datamanage it.
#'
#' @param df_chu The CHU dataframe.
#'
#' @return The datamanaged dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' ## CHU file
#' load(file = "extdata/COVID_ferte/dataCovidCHU.rdata")
#'
#' ImportCleanSIDEPCHU(df_chu = dataCovidCHU)
#'
#' }
ImportCleanSIDEPCHU <- function(df_chu){
  df_hospitPCR <- datagouv_importHospitPCR(df_chu = df_chu)
  ls_fctPastCum <- ls_fctmeanCasesDeriv()
  df_hospitPCRDM <- dm_createVar(df = df_hospitPCR,
                                 ls_fctPastCum = ls_fctPastCum)
  return(df_hospitPCRDM)
}
