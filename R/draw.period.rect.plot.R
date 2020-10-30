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
draw.period.rect.plot <- function(pre.period.start,
                                  pre.period.end,
                                  post.period.start,
                                  post.period.end,
                                  ssc.implementation.start,
                                  ssc.implementation.end) {
  
  time.period.dt = data.table(
    period.name = factor(x = c('pre', 'implementation', 'post'),
                         levels = c('pre', 'implementation', 'post'),
                         labels = c('Pre-SSC', 'Implementation', 'Post-SSC')),
    x.min = as.Date(c(
      pre.period.start,
      ssc.implementation.start,
      post.period.start
    )),
    x.max = as.Date(c(pre.period.end,
                      ssc.implementation.end,
                      post.period.end))
  )
  
  
  period.rect.plot = geom_rect(
    data = time.period.dt,
    mapping = aes(
      xmin = x.min,
      xmax = x.max,
      ymin = -Inf,
      ymax = Inf,
      fill = period.name
    ),
    alpha = 0.25,
    inherit.aes = F
  )
    
  
  return(period.rect.plot)
  
}
