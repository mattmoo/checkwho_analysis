##' Generate a list of 
##'
##' @title
##' @param input.dt
##' @param outcome
##' @param predictors
##' @param covariates
##' @param family
##' @param tau
generate.quantile.regression.models <-
  function(input.dt,
           outcome,
           predictors,
           covariates,
           tau) {
    
    reg.func = function(tau) {
      generate.regression.model(
        input.dt = input.dt,
        outcome = outcome,
        predictors = predictors,
        covariates = covariates,
        family = "quantile",
        tau = tau,
        method = 'fn'
      )
    }
    
    models = lapply(X = tau, FUN = reg.func)
    
    return(models)
    
  }
