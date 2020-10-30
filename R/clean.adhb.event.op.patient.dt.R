##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param adhb.event.op.patient.raw.dt
clean.adhb.event.op.patient.dt <- function(adhb.event.op.patient.raw.dt) {

  adhb.event.op.patient.dt = copy(adhb.event.op.patient.raw.dt)
  
  adhb.event.op.patient.dt[, `Event ID` := as.character(`Event ID`)]
  
  return(adhb.event.op.patient.dt)

}
