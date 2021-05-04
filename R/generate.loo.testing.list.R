##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param model.list.1 First model to be compared 
##' @param model.list.2
generate.loo.testing.list <- function(model.list.1,
                                      model.list.2) {

  
  if (length(model.list.1) != length(model.list.2)) {
    stop('Model lists are not of equal length.')
  }
  
  if (!all.equal(names(model.list.1), names(model.list.2))) {
    stop('Model list names do not match.')
  }
  
  loo.testing.list = list()
  
  for (model.ind in 1:length(model.list.1)) {
    model.name = names(model.list.1)[model.ind]
    
    loo.models = list(loo.1 = model.list.1[[model.ind]]$loo,
                      loo.2 = model.list.2[[model.ind]]$loo)
    loo.testing.list[[model.name]][['loo.models']] = loo.models
    
    loo.compare = loo::loo_compare(loo.models[['loo.1']], loo.models[['loo.2']])
    loo.compare.dt = data.table(loo.compare, keep.rownames = TRUE)
    setnames(loo.compare.dt, 'rn', 'model')
    setorder(loo.compare.dt, model)
    
    weights = loo::loo_model_weights(loo.models)
    loo.compare.dt[, weight := weights]
    
    loo.testing.list[[model.name]][['loo.compare']] = loo.compare.dt
    
    loo.testing.list[[model.name]]['z'] = loo.compare.dt[model == 'model2', elpd_diff/se_diff]
    loo.testing.list[[model.name]]['p'] = loo.compare.dt[model == 'model2', 2 * pnorm(elpd_diff/se_diff)]
    
    
  }
  
  
  return(loo.testing.list)
}
