##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param input.summary.dt
##' @param y.scale.power
draw.presentation.figure.daoh <- function(input.summary.dt,
                                          y.scale.root = 1,
                                          y.scale = NULL) {
  
  root_trans = scales::trans_new(
    "root_trans",
    transform = function(x) x^(1/y.scale.root),
    inverse = function(x)
      ifelse(x < 0, 0, x ^ y.scale.root),
    domain = c(0, Inf)
  )
  
  
  p = plot.daoh.barplot(input.summary.dt, daoh.col.name = 'daoh') +
    # labs(y = 'Percentage of group') +
    theme(legend.position = "none") +
    y.scale +
    coord_trans(y = root_trans)

  return(p)
}
