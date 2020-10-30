##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param adhb.revision.one.event.op.patient.raw.dt
clean.adhb.revision.one.event.op.patient.dt <- function(adhb.revision.one.event.op.patient.raw.dt) {

  adhb.revision.one.event.op.patient.dt = copy(adhb.revision.one.event.op.patient.raw.dt)
  
  adhb.revision.one.event.op.patient.dt[,`Unnamed: 11` := NULL]
  adhb.revision.one.event.op.patient.dt[,opdatetime := lubridate::as_datetime(lubridate::dmy_hm(opdate))]
  adhb.revision.one.event.op.patient.dt[,opdate := as.Date(opdatetime)]
  
  # # Columns for matching by opdate
  # adhb.revision.one.event.op.patient.dt[,match.start.opdate := opdate-1]
  # adhb.revision.one.event.op.patient.dt[,match.end.opdate := opdate+1]
  
  return(adhb.revision.one.event.op.patient.dt)

}
