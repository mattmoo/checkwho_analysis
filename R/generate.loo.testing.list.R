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
    
    loo.models = list(loo.1 = loo::loo(model.list.1[[model.ind]]),
                      loo.2 = loo::loo(model.list.2[[model.ind]]))
    loo.testing.list[[model.name]][['loo.models']] = loo.models
    
    loo.compare = loo::loo_compare(loo.models[['loo.1']], loo.models[['loo.2']])
    loo.compare.dt = data.table(loo.compare, keep.rownames = TRUE)
    setnames(loo.compare.dt, 'rn', 'model')
    setorder(loo.compare.dt, model)
    
    weights = loo::loo_model_weights(loo.models)
    loo.compare.dt[, weight := weights]
    
    loo.testing.list[[model.name]][['loo.compare']] = loo.compare.dt
    
    
  }
  
  
  return(loo.testing.list)
}
