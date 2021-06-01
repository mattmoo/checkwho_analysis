##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param input.dt
##' @param outcome
##' @param predictors
##' @param family 
generate.regression.model <- function(input.dt,
                                      outcome,
                                      predictors = 'SSC',
                                      covariates = c(), 
                                      family = "binomial",
                                      ...) {
  
  # Regression family variable cannot be called family because of a bug in jtools.
  # Actually it still doesn't work
  fam = family
  
  form = as.formula(paste(outcome, '~', '(', paste(c(predictors, covariates), collapse = "+"), ')'))
  
  if (fam == 'quantile') {
    model = rq(formula = form, 
               data = input.dt,
               ...)
    
  } else if (fam == 'binomial') {
    model = glm(formula = form, 
               data = input.dt,
               family = 'binomial',
               ...)
    
  } else if (fam == 'bernoulli.identity') {
    model = glm(formula = form, 
                data = input.dt,
                family = bernoulli(link = 'identity'),
                ...)
    
  } else {
    
    model = glm(formula = form, 
                data = input.dt, 
                family = fam,
                ...)
  }
  
  return(model)
  
}
