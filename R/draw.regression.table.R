##' Summarise regression using jtools
##'
##' @title
##' @param models A  regression model, or list of models.
##' @param coefs Coefficient names, as in jtools::export_summs.
##' @param groups Groups of factors to make separate graphs for.
draw.regression.table <- function(models,
                                  model.names = NULL,
                                  coefs = NULL) {
  
  t = export_summs(
    models,
    model.names = model.names,
    error_pos = 'right',
    coefs = coefs,
    results = 'asis',
    ci_level = 0.95, error_format = "[{conf.low}, {conf.high}]"
  )
  
  return(t)
}
