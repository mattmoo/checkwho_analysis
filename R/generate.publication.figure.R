##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param name Character name by which it will be referenced in
##'   reports.
##' @param input.plot ggplot2 input
##' @param caption The figure caption
##' @param width.proportion Proportion of page for width
##' @param aspect.ratio Aspect ratio (W/H)

generate.publication.figure <- function(name,
                                        input.plot, 
                                        caption, 
                                        width.proportion = 1,
                                        aspect.ratio = 1,
                                        theme = NULL) {

  
  theme = theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  )
  
  
  output = list(name = name,
                type = 'figure',
                caption = caption,
                ggplot = input.plot,
                width.proportion = width.proportion,
                aspect.ratio = aspect.ratio)
  
  if (!is.null(theme)) {
    output$ggplot = output$ggplot + theme
  }
  
  return(output)

}
