##' Summarise regression of a binary variable in a plot.
##'
##' @title
##' @param models A binomial regression model, or list thereof.
##' @param coefs Coefficient names, as in jtools::plot_summs.
##' @param model.names NAmes for labelling the models.
##' @param groups Groups of factors to make separate graphs for.
##' @param xbreaks Breaks on x-axis
##' @param ylabs Display y labels? You might want to remove them if there's only
##'   one variable.
draw.binary.regression.plot <- function(models, 
                                        coefs = NULL,
                                        model.names = NULL,
                                        groups = NULL,
                                        xbreaks = waiver(),
                                        ylabs = TRUE) {
  
  plot.func = function(mod,
                       plot.coefs) {
    result = plot_summs(
      mod,
      inner_ci_level = .9,
      coefs = plot.coefs,
      model.names = model.names,
      exp = TRUE
    ) +
      scale_x_continuous(name = 'Odds ratio',
                         breaks = xbreaks) +
      ylab(NULL) +
      theme_gray()
    
    return(result)
  }
  
  if (!is.null(groups)) {
    n.groups = 0
    plotlist = list()
    for (group in groups) {
      plot.coefs = coefs[coefs %in% group]
      if (length(coefs) > 0) {
        plotlist[[length(plotlist) + 1]] =
          plot.func(models, plot.coefs)
        
        
        n.groups = n.groups + 1
      }
    }
    
    p = ggarrange(plotlist = plotlist,
                  nrow = n.groups)
  } else {
    p = plot.func(models, coefs)
  }
  
  if (ylabs == FALSE) {
    p = p + theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank())
  }


  return(p)
}
