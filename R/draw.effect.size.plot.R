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
                                  box.scale = 0.2,
                                  hist.breaks = (seq(-10, 10, by = 0.125)),
                                  x.scale = scale_x_continuous(name = 'Effect size (DAOH)', breaks = seq(-10, 10, by =
                                                                                                           0.5))) {
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
    mcmc.dt[, iteration.id := 1:.N]
    return(mcmc.dt)
  }
  mcmc.dt = mcp.fit.to.mcmc.dt(mcp.fit)
  mcmc.dt[, cp.deflection := (date.numeric_2 - date.numeric_1) * (cp_2 - cp_1)]
  
  gradient.dt = melt(data = mcmc.dt,
                     measure.vars = c('date.numeric_1', 'date.numeric_2'))
  
  eliminate.tails = 0.05
  gradient.dt = gradient.dt[value >= as.numeric(quantile(value, probs = eliminate.tails)) &
                              value <= as.numeric(quantile(value, probs = 1 - eliminate.tails))]
  
  
  max.N = mcmc.dt[, .N, by = cut(cp.deflection, hist.breaks)][N == max(N), N][1]
  
  box.scale.actual = max.N / mcmc.dt[, .N] * box.scale
  
  x.scale$limits = mcmc.dt[, c(min(cp.deflection), max(cp.deflection))]
  
  p = ggplot(data = mcmc.dt, aes(x = cp.deflection)) +
    geom_vline(xintercept = 0, linetype = 'dashed') +
    # geom_density(fill = grDevices::rgb(0.5,0.5,0.5,0.5), aes(y = ..count..)) +
    geom_histogram(fill = grDevices::rgb(0.5, 0.5, 0.5, 0.5),
                   breaks = hist.breaks,
                   aes(y = (..count..) / sum(..count..))) +
    # geom_boxplot(outlier.alpha = 0, outlier.size = NA) +
    stat_boxplot(geom = "errorbar",
                 width = box.scale.actual / 2,
                 size = 0.5) +
    geom_boxplot(
      # lwd = 0.1,
                 outlier.alpha = 0,
                 width = box.scale.actual) +
    x.scale +
    scale_y_continuous(
      name = paste0(
        'Percentage of ',
        prettyNum(mcmc.dt[, .N], big.mark = ','),
        ' iterations'
      ),
      labels = function(x) scales::percent(x, accuracy = 1),
      breaks = seq(0, 1, by = 0.05)
    )
  
  return(p)
}
