##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param monthly.summary.dt
##' @param period.rect.plot
draw.daoh.exceedance.time.plot <- function(summary.dt,
                                           period.rect.plot = NULL) {

  quantile.sum.dt = rbindlist(list(
    data.table = summary.dt[, .(daoh.start,
                                exceedance.proportion = exceeds.daoh.10,
                                Percentile = scales::percent(.1))],
    
    data.table = summary.dt[, .(daoh.start,
                                exceedance.proportion = exceeds.daoh.25,
                                Percentile = scales::percent(.25))],
    
    data.table = summary.dt[, .(daoh.start,
                                exceedance.proportion = exceeds.daoh.median,
                                Percentile = scales::percent(.5))],
    
    data.table = summary.dt[, .(daoh.start,
                                exceedance.proportion = exceeds.daoh.75,
                                Percentile = scales::percent(.75))]
    # ,
    # 
    # data.table = summary.dt[, .(daoh.start,
    #                             exceedance.proportion = exceeds.daoh.90,
    #                             Percentile = scales::percent(.9))]
  ))
  
  x.breaks = seq(2004,2014,by = 1)
  
  y.breaks = seq(from = 0, to = 1, by = 0.05)

  p = ggplot()
  
  if (!is.null(period.rect.plot)) {
    p = p + period.rect.plot
  }
  
  p = p + 
    geom_line(data = quantile.sum.dt,
              mapping = aes(x = daoh.start,
                            y = exceedance.proportion,
                            group = Percentile,
                            linetype = Percentile)) +
    geom_point(data = quantile.sum.dt,
               mapping = aes(x = daoh.start,
                             y = exceedance.proportion,
                             group = Percentile,
                             shape = Percentile)) +
    scale_y_continuous(name = 'Mortality',
                       labels = scales::percent, 
                       limits = c(0,1),
                       breaks = y.breaks) +
    scale_x_date(name = 'Year of surgery',
                 # breaks = x.breaks,
                 minor_breaks = NULL) +
    theme(axis.text.x = element_text(angle = 90, 
                                     vjust = 0.5, 
                                     hjust=1))
  
  
  
  return(p)
  
}
