##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param summary.dt Outcomes summarised by some time metric, with proportions of mortality.
draw.mortality.time.plot <- function(summary.dt,
                                     period.rect.plot = NULL) {
  
  mort.sum.dt = rbindlist(list(summary.dt[, .(daoh.start,
                                              proportion = mort.30.day,
                                              mort.type = '30 day')],
                               
                               summary.dt[, .(daoh.start,
                                              proportion = mort.90.day,
                                              mort.type = '90 day')]))
  
  x.breaks = seq(2004,2014,by = 1)
  
  y.breaks = seq(from = 0, to = 1, by = 0.025)
  # 
  p = ggplot(mort.sum.dt, aes(x = daoh.start,
                              y = proportion,
                              group = mort.type))
  
  p = ggplot()
  
  if (!is.null(period.rect.plot)) {
    p = p + period.rect.plot
  }
  
  p = p + 
    geom_line(data = mort.sum.dt,
              aes(x = daoh.start,
                  y = proportion,
                  group = mort.type,
                  linetype = mort.type)) +
    geom_point(data = mort.sum.dt,
               aes(x = daoh.start,
                   y = proportion,
                   group = mort.type,
                   shape = mort.type)) +
    scale_y_continuous(name = 'Mortality',
                       labels = scales::percent, 
                       breaks = y.breaks) +
    scale_x_date(name = 'Year of surgery',
                 # breaks = x.breaks,
                 minor_breaks = NULL) +
    theme(axis.text.x = element_text(angle = 90, 
                                     vjust = 0.5, 
                                     hjust=1))
  

  
  return(p)

}
