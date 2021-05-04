##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param mcp.fit
##' @param prior
##' @param x.label
draw.effect.size.plot <- function(mcp.fit,
                                  prior = FALSE,
                                  x.label = "Effect size (logit)") {
  

  
  # Sample prior or posterior.
  if (prior == FALSE) {
    mcmc = mcp.fit$mcmc_post
  } else {
    mcmc = mcp.fit$mcmc_prior
  }
  
  # Turn MCMC into plottable data.table
  mcp.fit.to.mcmc.dt = function(mcp.fit) {
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
  mcmc.dt = mcp.fit.to.mcmc.dt(mcp.fit)
  mcmc.dt[, cp.deflection := (date.numeric_2 - date.numeric_1) * (cp_2 - cp_1)]
  
  gradient.dt = melt(data = mcmc.dt, measure.vars = c('date.numeric_1', 'date.numeric_2'))
  
  eliminate.tails = 0.05
  gradient.dt = gradient.dt[value >= as.numeric(quantile(value, probs = eliminate.tails)) & value <= as.numeric(quantile(value, probs = 1 - eliminate.tails))]
  
  p = ggplot(data = mcmc.dt, aes(x = cp.deflection)) +
    geom_vline(xintercept = 0, linetype = 'dashed') +
    geom_density(fill = grDevices::rgb(0.5,0.5,0.5,0.5)) +
    geom_boxplot(outlier.alpha = 0.1) +
    scale_x_continuous(name = x.label, breaks = seq(-10,10,by=0.1)) +
    scale_y_continuous(name = 'Density', breaks = seq(0,100,by=1))
    
  return(p)
}
