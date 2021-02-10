##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param pre.post.figure.dt
##' @param dra
generate.daoh.pre.post.summary.dt <- function(pre.post.figure.dt, 
                                              dra = TRUE) {


  if (dra == TRUE) {
    daoh.pre.post.summary.dt = calculate.summary.dt(
      pre.post.figure.dt,
      by.group = 'SSC',
      weight.col.name = 'SSC.dra.weight'
    )
  } else {
    daoh.pre.post.summary.dt = calculate.summary.dt(
      pre.post.figure.dt,
      by.group = 'SSC'
    )
  }
  daoh.pre.post.summary.dt[, adjustment := ifelse(dra,
                                                 yes = 'DRA',
                                                 no = 'Raw')]
  daoh.pre.post.summary.dt[, adjustment == factor(adjustment, levels = c('Raw', 'DRA'))]
  
  return(daoh.pre.post.summary.dt)

}
