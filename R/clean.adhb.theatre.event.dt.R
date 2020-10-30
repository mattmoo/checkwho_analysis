##' Takes ADHB theatre events from provided wide format to long format by Event ID.
##'
##' .. content for \details{} ..
##'
##' @title
##' @param adhb.event.op.patient.dt  Slightly cleaned raw data in data.table
##'   format, identifiable data encrypted in Python, including combined event,
##'   operation, and patient data provided by ADHB
clean.adhb.theatre.event.dt <- function(adhb.event.op.patient.dt) {

  adhb.theatre.event.dt = adhb.event.op.patient.dt[,
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
  adhb.theatre.event.dt = unique(adhb.theatre.event.dt)
  
  adhb.theatre.event.dt[,`Actual Into Theatre Date Time` := lubridate::as_datetime(`Actual Into Theatre Date Time`)]
  adhb.theatre.event.dt[,`Actual Into Theatre Date` := as.Date(`Actual Into Theatre Date Time`)]
  
  
  return(adhb.theatre.event.dt)
}
