##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param smooth.daoh.summary.dt
generate.daoh.scaling.dt <- function(smooth.daoh.summary.dt,
                                     inverse.emp.logit) {

  daoh.scaling.dt = merge(x = smooth.daoh.summary.dt[measure == 'daoh', .(time, fit.orig = fit, se.fit.orig = se.fit)],
                          y = smooth.daoh.summary.dt[measure == 'daoh.emp.logit', .(time, fit.inv = inverse.emp.logit(fit), se.fit.inv = inverse.emp.logit(se.fit))],
                          by = 'time')
  daoh.scaling.dt[, ratio := fit.inv / fit.orig]
  
  return(daoh.scaling.dt)

}
