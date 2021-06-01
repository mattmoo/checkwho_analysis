
# Turn MCMC into plottable data.table
mcp.fit.to.mcmc.dt = function(mcp.fit, prior = FALSE) {
  
  # Sample prior or posterior.
  if (prior == FALSE) {
    mcmc = mcp.fit$mcmc_post
  } else {
    mcmc = mcp.fit$mcmc_prior
  }
  
  mcmc.dt = data.table()
  for (chain.ind in 1:length(mcmc)) {
    chain.dt = data.table(mcmc[[chain.ind]])
    chain.dt[, chain := factor(chain.ind)]
    mcmc.dt = rbindlist(list(mcmc.dt,
                             chain.dt))
  }
  mcmc.dt[,iteration.id := 1:.N]
  return(mcmc.dt)
}