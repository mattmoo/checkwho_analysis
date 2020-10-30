##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param moh.revision.one.patient.raw.dt
clean.moh.revision.one.patient.dt <- function(moh.revision.one.patient.raw.dt) {
  
  moh.revision.one.patient.dt = copy(moh.revision.one.patient.raw.dt)
  
  setnames(moh.revision.one.patient.dt, old ='nhi_dod', new = 'date_of_death')
  
  daohtools::convert.date.cols(moh.revision.one.patient.dt,
                               date.col.names = c("date_of_death"))
  
  return(moh.revision.one.patient.dt)
}
