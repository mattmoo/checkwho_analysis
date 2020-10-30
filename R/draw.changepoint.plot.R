##' Draws a plot with all of the changepoint analyses
##'
##' @title
##' @param changepoint.model.list List of changeppint models.
##' @param changepoint.measure.name.list Equal sized list of names.
##' @param period.rect.plot Optional ggplot item to highlight periods.
draw.changepoint.plot <- function(changepoint.model.list,
                                  changepoint.measure.name.list, 
                                  period.rect.plot = NULL) {

  start.month = min(changepoint.model.list[[1]]$data$month)
  end.month = max(changepoint.model.list[[1]]$data$month)
  
  month.breaks = as.numeric(seq.Date(from = start.month,
                                     to = end.month,
                                     by = "3 months"))
  year.breaks = seq.Date(from = floor_date(start.month, unit = 'years'),
                         to = floor_date(end.month, unit = 'years'),
                         by = "1 year")
  
  plot.list = list()
  
  for (fit.ind in 1:length(changepoint.model.list)) {
    
    fit = changepoint.model.list[[fit.ind]]
    
    if (fit.ind != length(changepoint.model.list)) {
      year.breaks.it = NULL
    } else {
      year.breaks.it = year.breaks
    }
    
    p = plot(fit) +
      scale_y_continuous(labels = scales::percent, name = element_blank()) +
      scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%Y"),
                         breaks = year.breaks.it,
                         minor_breaks = month.breaks,
                         name = element_blank())
    
    if (!is.null(period.rect.plot)) {
      p = p + period.rect.plot
    }
    
    plot.list[[fit.ind]] = p
  }
  
  p = ggarrange(plotlist = plot.list,
                labels = changepoint.measure.name.list,
                nrow = fit.ind)
  
  return(p)

}
