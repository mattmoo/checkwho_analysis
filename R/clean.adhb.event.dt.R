##' Extracts and cleans event level data from ADHB.
##'
##' Converts dates.
##'
##' @title
##' @param adhb.event.op.patient.dt  Slightly cleaned raw data in data.table
##'   format, identifiable data encrypted in Python, including combined event,
##'   operation, and patient data provided by ADHB
clean.adhb.event.dt = function(adhb.event.op.patient.dt) {
  
  
  adhb.event.dt = adhb.event.op.patient.dt[, c("Event ID",
                                               "NHI",
                                               "Admit Date",
                                               "Discharge Date",
                                               "Cases Completed"), with = FALSE]
  
  # daohtools::convert.date.cols(adhb.event.dt, date.col.names = c("Admit Date",
  #                                                                "Discharge Date"))

  
  adhb.event.dt = unique(adhb.event.dt)
  
  return(adhb.event.dt)
}
