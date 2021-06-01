##' Make a list of parameters for a list of mcp models.
##'
##' @title
##' @param mcp.fit.list
generate.factor.mcmc.dt <- function(mcp.fit.list, 
                                       prior = FALSE, 
                                       label.col.name = 'model',
                                       label.factor.order = NULL) {

  mcmc.dt = data.table()
  for (mcp.fit.ind in 1:length(mcp.fit.list)) {
    label.name = names(mcp.fit.list)[mcp.fit.ind]
    mcp.fit = mcp.fit.list[[mcp.fit.ind]]
    it.mcmc.dt = mcp.fit.to.mcmc.dt(mcp.fit, prior = prior)
    set(it.mcmc.dt, j = label.col.name, value = label.name)
    set(it.mcmc.dt, j = 'prior', value = prior)
    
    mcmc.dt = rbindlist(list(mcmc.dt, it.mcmc.dt))
    
  }
  
  return(mcmc.dt)
  
}
