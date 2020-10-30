##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param moh.diag.op.dt
##' @param diag.lookup.dt
generate.moh.diag.dt <- function(moh.diag.op.dt, diag.lookup.dt) {

  moh.diag.dt = moh.diag.op.dt[DIAG_TYP != 'O']
  
  moh.diag.dt = merge(x = moh.diag.dt,
                      y = diag.lookup.dt,
                      by.x = 'CLIN_CD',
                      by.y = 'code.diagnosis',
                      all.x = TRUE)
  
  return(moh.diag.dt)
}
