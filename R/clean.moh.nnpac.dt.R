##' Cleans non-admitted patient data provided by MOH.
##'
##' Mainly converts dates.
##'
##' @title
##' @param moh.nnpac.raw.dt Raw data in data.table format, identifiable data encrypted in Python.
clean.moh.nnpac.dt <- function(moh.nnpac.raw.dt) {
  
  moh.nnpac.dt = copy(moh.nnpac.raw.dt)
  
  daohtools::convert.date.cols(moh.nnpac.dt, date.col.names = 'date_of_service')

  return(moh.nnpac.dt)

}
