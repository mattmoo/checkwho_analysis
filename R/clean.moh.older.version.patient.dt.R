##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param moh.older.version.patient.raw.dt
clean.moh.older.version.patient.dt <- function(moh.older.version.patient.raw.dt) {
  
  moh.older.version.patient.dt = copy(moh.older.version.patient.raw.dt)
  
  setnames(moh.older.version.patient.dt, old ='nhi_dod', new = 'date_of_death')
  
  daohtools::convert.date.cols(moh.older.version.patient.dt,
                               date.col.names = c("date_of_death"))
  
  return(moh.older.version.patient.dt)
}
