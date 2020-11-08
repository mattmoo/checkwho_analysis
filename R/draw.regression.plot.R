##' Summarise regression of a binary variable in a plot.
##'
##' @title
##' @param models A binomial regression model, or list thereof.
##' @param coefs Coefficient names, as in jtools::plot_summs.
##' @param model.names NAmes for labelling the models.
##' @param groups Groups of factors to make separate graphs for.
##' @param xbreaks Breaks on x-axis
##' @param xlab Label for x-axis
##' @param ylabs Display y labels? You might want to remove them if there's only
##'   one variable.
##' @param ... More arguments for plot_summs
draw.regression.plot <- function(models,
                                 coefs = NULL,
                                 model.names = NULL,
                                 groups = NULL,
                                 xbreaks = waiver(),
                                 xlab = NULL,
                                 ylabs = TRUE,
                                 ...) {
  
  # Plot function for applying to each model.
  plot.func = function(mod,
                       plot.coefs) {
    
    

    
    if ("glm" %in% class(mod) &&
        mod$family$link == 'logit') {
      exp = TRUE
      if (is.null(xlab)) {
        xlab = 'Odds ratio'
      }
    } else {
      exp = FALSE
      if (is.null(xlab)) {
        xlab = element_blank()
      }
    }
    
    result = plot_summs(
      mod,
      inner_ci_level = .9,
      coefs = plot.coefs,
      model.names = model.names,
      exp = exp,
      ...
    ) +
      scale_x_continuous(name = xlab,
                         breaks = xbreaks) +
      ylab(NULL) +
      theme_gray()
    
    return(result)
  }
  
  # Group into separate graphs if requested.
  if (!is.null(groups)) {
    n.groups = 0
    plotlist = list()
    for (group in groups) {
      plot.coefs = coefs[coefs %in% group]
      if (length(coefs) > 0) {
        
        # Convert to list if required.
        if (!'list' %in% class(models)) {
          models = list(models)
        }
        # Rename only present coefficients to avoid error.
        old.plot.coefs = plot.coefs
        plot.coefs = plot.coefs[plot.coefs %in% unique(unlist(lapply(X = models, FUN = function(x) names(x$coefficients))))]
        
        if (length(plot.coefs) == 0) {
          warning(paste0('No present coefficients found in: '),
                  paste(old.plot.coefs, collapse = ', '))
        } else {
          plotlist[[length(plotlist) + 1]] =
            plot.func(models, plot.coefs)
          
          
          n.groups = n.groups + 1
        }
        
      }
    }
    
    p = ggarrange(plotlist = plotlist,
                  nrow = n.groups,
                  common.legend = TRUE,
                  legend = "bottom")
  } else {
    # If no groups, just plot it.
    p = plot.func(models, coefs)
  }
  
  if (ylabs == FALSE) {
    p = p + theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank())
  }
  
  
  
  return(p)
}
