##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param adhb.revision.one.recalculated.event.op.patient.dt
clean.adhb.revision.one.recalculated.event.dt <- function(adhb.revision.one.recalculated.event.op.patient.dt) {

  # # Doesn't actually have AdmitDate
  # adhb.revision.one.recalculated.event.dt =
  #   adhb.revision.one.recalculated.event.op.patient.dt[, .(`Event ID` = EventNumber,
  #                                                          NHI = nhi)]
  adhb.revision.one.recalculated.event.dt =
    adhb.revision.one.recalculated.event.op.patient.dt[, c("Event ID",
                                                           "NHI",
                                                           "Admit Date",
                                                           "Discharge Date",
                                                           "Cases Completed"), with = FALSE]
  # daohtools::convert.date.cols(adhb.event.dt, date.col.names = c("Admit Date",
  #                                                                "Discharge Date"))
  
  
  adhb.revision.one.recalculated.event.dt = unique(adhb.revision.one.recalculated.event.dt)
  
  return(adhb.revision.one.recalculated.event.dt)

}
