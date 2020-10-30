##' Cleans non-admitted patient data provided by MOH.
##'
##' Mainly converts dates.
##'
##' @title
##' @param moh.event.raw.dt Raw data in data.table format, identifiable data encrypted in Python.
clean.moh.event.dt <- function(moh.event.raw.dt) {
  
  moh.event.dt = copy(moh.event.raw.dt)
  
  moh.event.dt[, FACILITY := as.character(FACILITY)]
  moh.event.dt[, EVENT_ID := as.character(EVENT_ID)]
  
  moh.event.dt[, CCI := as.character(PCCL)]
  moh.event.dt[CCI %in% c("0","1"), CCI := "0-1"]
  moh.event.dt[, CCI := factor(CCI, levels = c("0-1","2","3","4"))]
  
  daohtools::convert.date.cols(moh.event.dt, date.col.names = c("EVSTDATE", "EVENDATE"))
  
  return(moh.event.dt)
  
}