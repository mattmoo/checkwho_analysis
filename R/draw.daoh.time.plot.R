##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param monthly.summary.dt
##' @param ssc.implementation.start
##' @param ssc.implementation.end
draw.daoh.time.plot <- function(summary.dt,
                                period.rect.plot = NULL) {

  quantile.sum.dt = rbindlist(list(
    data.table = summary.dt[, .(daoh.start,
                                DAOH = daoh.10,
                                Percentile = scales::percent(.1))],
    
    data.table = summary.dt[, .(daoh.start,
                                DAOH = daoh.25,
                                Percentile = scales::percent(.25))],
    
    data.table = summary.dt[, .(daoh.start,
                                DAOH = daoh.median,
                                Percentile = scales::percent(.5))],
    
    data.table = summary.dt[, .(daoh.start,
                                DAOH = daoh.75,
                                Percentile = scales::percent(.75))],
    
    data.table = summary.dt[, .(daoh.start,
                                DAOH = daoh.90,
                                Percentile = scales::percent(.9))]
  ))
  
  x.breaks = seq(2004,2014,by = 1)
  y.breaks = seq(from = 0, to = 90, by = 5)
  
  
  p = ggplot()
  
  if (!is.null(period.rect.plot)) {
    p = p + period.rect.plot
  }
  
  p = p + 
    geom_line(data = quantile.sum.dt,
              aes(x = daoh.start, 
                  y = DAOH, 
                  group = Percentile,
                  linetype = Percentile)) +
    geom_point(data = quantile.sum.dt,
               aes(x = daoh.start, 
                   y = DAOH, 
                   group = Percentile,
                   shape = Percentile)) +
    scale_y_continuous(name = 'DAOH',
                       limits = c(0,90),
                       breaks = y.breaks) +
    # scale_x_continuous(name = 'Year of admission', 
    #                    breaks = x.breaks, 
    #                    minor_breaks = NULL) +
    scale_x_date(name = 'Year of surgery',
                 # breaks = x.breaks,
                 minor_breaks = NULL) +
    theme(axis.text.x = element_text(angle = 90, 
                                     vjust = 0.5, 
                                     hjust=1))
  
  return(p)

}
