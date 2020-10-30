##' Takes patient level data out of provided ADHB data
##'
##' .. content for \details{} ..
##'
##' @title
##' @param adhb.event.op.patient.dt  Slightly cleaned raw data in data.table
##'   format, identifiable data encrypted in Python, including combined event,
##'   operation, and patient data provided by ADHB
clean.adhb.patient.dt <- function(adhb.event.op.patient.dt) {
  
  adhb.patient.dt = unique(adhb.event.op.patient.dt[, .(NHI, `Gender Desc`, `Date of Birth`)])
  
  # daohtools::convert.date.cols(adhb.patient.dt, date.col.names = c("Date of Birth"))

  return(adhb.patient.dt)
}
