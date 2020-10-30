##' Cleans diagnosis, operation, and cause data provided by MOH.
##'
##' Mainly converts dates.
##'
##' @title
##' @param moh.diag.raw.dt Raw data in data.table format, identifiable data encrypted in Python.
clean.moh.diag.op.dt = function(moh.diag.op.raw.dt) {
  
  moh.diag.op.dt = copy(moh.diag.op.raw.dt)
  
  daohtools::convert.date.cols(moh.diag.op.dt, date.col.names = 'OP_ACDTE')
  
  return(moh.diag.op.dt)

}
