##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param pre.period.start
##' @param pre.period.end
##' @param post.period.start
##' @param post.period.end
##' @param ssc.implementation.start
##' @param ssc.implementation.end
draw.period.vline.plot <- function(pre.period.start,
                                  pre.period.end,
                                  post.period.start,
                                  post.period.end,
                                  ssc.implementation.start,
                                  ssc.implementation.end) {
  
  
  period.vline.plot = geom_vline(xintercept = as.Date(
    c(pre.period.start, 
      post.period.end, 
      ssc.implementation.start,
      ssc.implementation.end)
  ),
  linetype = c('dotted', 'dotted', 'dashed', 'dashed'))
    
  
  return(period.vline.plot)
  
}
