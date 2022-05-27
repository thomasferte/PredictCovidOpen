#' COVID19 Lockdown and curfews
#'
#'
#' The dataframe contains the following columns:
#' \itemize{
#'   \item NPI The Non Pharmaceutical Intervention
#'   \item start The start date of the intervention
#'   \item end The end date of intervention
#' }
#' @format A data frame with 8 rows and 3 variables
#'
#' @docType data
#'
#' @usage data(dfLockdownCurfewRaw)
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' dfLockdownCurfewRaw <- tribble(
#'   ~NPI, ~start, ~end,
#'   "lockdown1", "17/03/2020", "11/05/2020",
#'   "lockdown2", "30/10/2020", "15/12/2020",
#'   "lockdown3", "03/04/2021", "03/05/2021",
#'   "curfew20h", "15/12/2020", "16/01/2021",
#'   "curfew18h", "16/01/2021", "20/03/2021",
#'   "curfew19h", "20/03/2021", "03/04/2021",
#'   "curfew19hv2", "03/05/2021", "19/05/2021",
#'   "curfew21h", "19/05/2021", "09/06/2021",
#'   "curfew23h", "09/06/2021", "20/06/2021",
#' ) %>%
#'   mutate(across(.cols = c("start", "end"),
#'                 .fns = ~as.Date(x = .x, "%d/%m/%Y")))
#'
#' dfDate <- data.frame(date = seq(as.Date("2021-01-01"), as.Date("2021-04-01"), by = 1)) %>%
#'   mutate(HOSP = rnorm(n=nrow(.)))
#'
#' ggplot(dfDate, mapping = aes(x = date, y = HOSP)) +
#'   geom_rect(data = dfLockdownCurfewRaw,
#'             mapping = aes(xmin = start, xmax = end, ymin = min(dfDate$HOSP),
#'                           ymax = max(dfDate$HOSP),
#'                           fill = NPI),
#'             alpha = 0.5,
#'             inherit.aes = F) +
#'   geom_line()
#'
#' save(dfLockdownCurfewRaw, file = "data/dfLockdownCurfewRaw.rdata")
#' }
"dfLockdownCurfewRaw"
