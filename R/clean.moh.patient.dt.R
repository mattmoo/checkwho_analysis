##' Cleans patient data provided by MOH.
##'
##' Mainly converts dates.
##'
##' @title
##' @param moh.patient.raw.dt Raw data in data.table format, identifiable data encrypted in Python.
clean.moh.patient.dt = function(moh.patient.raw.dt) {

  moh.patient.dt = copy(moh.patient.raw.dt)
  
  daohtools::convert.date.cols(moh.patient.dt,
                               date.col.names = c("date_of_death",
                                                  "date_of_birth"))
  
  return(moh.patient.dt)
}
