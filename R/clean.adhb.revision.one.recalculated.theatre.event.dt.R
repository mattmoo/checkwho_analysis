##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param adhb.revision.one.recalculated.event.op.patient.dt
clean.adhb.revision.one.recalculated.theatre.event.dt <- function(adhb.revision.one.recalculated.event.op.patient.dt) {

  adhb.revision.one.theatre.event.dt = adhb.revision.one.recalculated.event.op.patient.dt[,
                                                   c(
                                                     "Theatre Event ID",
                                                     "Event ID",
                                                     "Actual Into Theatre Date Time",
                                                     "Theatre Suite",
                                                     "Cases Completed",
                                                     "Theatre Desc",
                                                     "Primary Proc",
                                                     "Secondary Proc"
                                                   ),
                                                   with = FALSE]
  adhb.revision.one.theatre.event.dt = unique(adhb.revision.one.theatre.event.dt)
  
  adhb.revision.one.theatre.event.dt[,`Actual Into Theatre Date Time` := lubridate::as_datetime(`Actual Into Theatre Date Time`)]
  adhb.revision.one.theatre.event.dt[,`Actual Into Theatre Date` := as.Date(`Actual Into Theatre Date Time`)]
  
  
  return(adhb.revision.one.theatre.event.dt)
}
