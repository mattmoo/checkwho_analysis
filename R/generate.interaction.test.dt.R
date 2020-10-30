##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param models
generate.interaction.test.dt <- function(models) {

  if (!('list' %in% class(models))) {
    models = list(models)
  }
  
  interaction.test.dt = data.table()
  
  # Go through the models
  for (model in models) {
    
    
    # Get parameters
    input.dt = as.data.table(model$model)
    outcome = names(model$model)[1]
    predictor = labels(terms(model))[1]
    covariates = labels(terms(model))[-1]
    
    # Go through the 
    for (covariate in covariates) {
      interaction.term = paste0(predictor, '*', covariate)
      
      if ('rq' %in% class(model)) {
        
        tau = model$tau
        
        new.model = generate.quantile.regression.models(
          input.dt = input.dt,
          outcome = outcome,
          predictors = predictor,
          covariates = c(covariates, interaction.term),
          tau = tau
        )
        
        w = anova(model, new.model[[1]], test = "Wald")
        
        row.dt = cbind(
          data.table(
            outcome = outcome,
            covariate = covariate,
            interaction.term = interaction.term,
            tau = tau
          ),
          data.table(w$table)
        )
        
      } else if ('glm' %in% class(model)) {
        new.model = generate.regression.model(
          input.dt = input.dt,
          outcome = outcome,
          predictors = predictor,
          covariates = c(covariates, interaction.term)
        )
        
        w = lrtest(model, new.model)
        
        row.dt =
          data.table(
            outcome = outcome,
            covariate = covariate,
            interaction.term = interaction.term,
            ndf = w[2, "Df"],
            Chisq = w[2, "Chisq"],
            pvalue = w[2, "Pr(>Chisq)"]
          )
      }
      
      
      interaction.test.dt = rbindlist(list(interaction.test.dt,
                                           row.dt),
                                      fill = TRUE)
    }
    
  }
  
  interaction.test.dt[, pvalue.fdr := p.adjust(pvalue, method = "fdr")]
  
  return(interaction.test.dt)

}
