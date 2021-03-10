##' Draws a plot illustrating a changepoint analysis.
##'
##' @title
##' @param mcp.fit mcp changepoint model.
##' @param smooth.summary.dt Smoothed summary data.table (see generate.smooth.summary.dt).
##' @param density.height Height of the density plot in y units (default: half of limits).
##' @param period.rect.plot Optional ggplot item to highlight periods (Default: NULL).
##' @param y.scale Optional ggplot continuous scale for y axis (Default: NULL).
##' @param y.scale Optional number of lines (and hence changepoints) to illustrate (Default: 200).
draw.changepoint.plot <- function(mcp.fit,
                                  smooth.summary.dt,
                                  density.height = NULL,
                                  period.rect.plot = NULL,
                                  y.scale = NULL,
                                  x.time.scale = NULL,
                                  n.lines = 200,
                                  ribbon.transparency = 0.15,
                                  ribbon.colour = grDevices::rgb(0, 0, 0, ribbon.transparency),
                                  rev.fill.scale = FALSE) {
  
  mcp.fit.to.mcmc_post.dt = function(mcp.fit) {
    mcmc_post.dt = data.table()
    for (chain.ind in 1:length(mcp.fit$mcmc_post)) {
      chain.dt = data.table(mcp.fit$mcmc_post[[chain.ind]])
      chain.dt[, chain := factor(chain.ind)]
      mcmc_post.dt = rbindlist(list(mcmc_post.dt,
                                    chain.dt))
    }
    return(mcmc_post.dt)
  }
  
  mcmc_post.dt = mcp.fit.to.mcmc_post.dt(mcp.fit)
  
  
  p = ggplot() +
    x.time.scale +
    y.scale +
    geom_line(data = smooth.summary.dt,
              aes(x = time,
                  y = fit),
              size = 1) +
    period.rect.plot +
    geom_ribbon(
      data = smooth.summary.dt,
      aes(x = time,
          ymin = ci.low,
          ymax = ci.high),
      fill = ribbon.colour,
      colour = 'black',
      linetype = 'dashed'
    )
  
  
  min.date = readd(min.date)
  max.date = readd(max.date)
  by.date = '1 day'
  mcp.fit = mcp.fit
  
  
  # binom.fam = FALSE
  # if (mcp.fit$family$family == 'binomial') {
  #   binom.fam = TRUE
  # }
  
  fit = mcp.fit
  
  facet_by = NULL
  n.lines = 200
  geom_data = "point"
  cp_dens = TRUE
  q_fit = FALSE
  q_predict = FALSE
  rate = TRUE
  prior = FALSE
  which_y = "ct"
  arma = TRUE
  nsamples = 2000
  scale = "response"
  
  xvar = rlang::sym(fit$pars$x)
  yvar = rlang::sym(fit$pars$y)
  is_arma = length(fit$pars$arma) > 0
  newdata = tibble::tibble(!!xvar := mcp:::get_eval_at(fit, facet_by = NULL))
  varying_pars = FALSE
  if (is_arma)
    newdata$.ydata = fit$data[, fit$pars$y]
  
  var.name = mcp.fit$pars$y
  if (fit$family$family == "binomial") {
    # Interpolate trials for binomial at all xvar to make sure that there are actually values to plot
    newdata[, fit$pars$trials] = suppressWarnings(stats::approx(
      x = fit$data[, fit$pars$x],
      y = fit$data[, fit$pars$trials],
      xout = dplyr::pull(newdata, xvar)
    )$y) %>%
      round()  # Only integers
    
    var.name = 'x'
  }
  
  local_pp_eval = function(type) {
    mcp:::pp_eval(
      object = fit,
      newdata = newdata,
      summary = FALSE,
      # Get samples
      type = type,
      rate = rate,
      prior = prior,
      which_y = which_y,
      varying = varying_pars,
      arma = arma,
      nsamples = nsamples,
      samples_format = "tidy",
      scale = scale
    ) %>%
      dplyr::rename(!!yvar := !!type)  # from "predict"/"fitted" to yvar (response name)
  }
  
  # Get data with fitted values. Optionally add predictions.
  samples_expanded = local_pp_eval("fitted")
  if (any(q_predict != FALSE))
    samples_expanded$.predicted = local_pp_eval("predict") %>% dplyr::pull(yvar)
  
  
  
  ###############################
  # PREP RESPONSE DATA FOR PLOT #
  ###############################
  ydata = fit$data[, fit$pars$y]  # Convenient shortname
  
  # If this is a binomial rate, divide by the number of trials
  if (fit$family$family == "binomial" && rate == TRUE)
    ydata = ydata / fit$data[, fit$pars$trials]
  
  # Show data
  if (scale == "linear") {
    ydata = fit$family$linkfun(ydata)
    if (any(is.infinite(ydata)))
      message("Removing points with infinite values on the linear scale. You may get a few warnings.")
    ydata[is.infinite(ydata)] = NA
  }
  
  # If this is time series, strip fit$data$y for the "ts" class to avoid ggplot2 warning about scale picking..
  # TO DO: hack.
  fit$data[, fit$pars$y] = as.numeric(ydata)
  
  data_lines.dt = as.data.table(tidybayes::sample_draws(samples_expanded, n.lines))  # Only this number of lines
  data_lines.control.dt = copy(data_lines.dt)
  data_lines.control.dt[date.numeric >= cp_1 &
                          date.numeric <= cp_2, x := NA]
  data_lines.intervention.dt = data_lines.dt[date.numeric >= cp_1 &
                                               date.numeric <= cp_2]
  
  changepoint.dt = data_lines.intervention.dt[, .SD[abs(cp_1 - date.numeric) == min(abs(cp_1 - date.numeric))], by = .iteration]
  changepoint.dt[, cp.deflection := (date.numeric_2 - date.numeric_1) * (cp_2 - cp_1)]
  changepoint.dt[, cp.deflection.sign := cp.deflection / abs(cp.deflection)]
  changepoint.dt[cp.deflection > -0.05 &
                   cp.deflection < 0.05, cp.deflection.sign := 0]
  changepoint.dt[, cp.deflection.sign := factor(cp.deflection.sign, levels = c(-1, 0, 1))]
  
  if (rev.fill.scale == TRUE) {
    effect.fill.scale = scale_fill_continuous_divergingx(
      name = 'Effect size',
      palette = 'RdBu',
      rev = TRUE,
      mid = 0,
      na.value = "black",
      labels = function(x)
        sprintf("%+.1f", x)
    )
  } else {
    effect.fill.scale = scale_fill_continuous_divergingx(
      name = 'Effect size',
      palette = 'RdBu',
      rev = FALSE,
      mid = 0,
      na.value = "black",
      labels = function(x)
        sprintf("%+.1f", x)
    )
  }
  
  
  p = p + geom_line(
    data = data_lines.control.dt,
    aes(
      group = .data$.draw,
      x = date.numeric,
      y = get(var.name)
    ),
    color = grDevices::rgb(0, 0, 0, 0.1)
  ) +
    geom_line(
      data = data_lines.intervention.dt,
      aes(
        group = .data$.draw,
        x = date.numeric,
        y = get(var.name)
      ),
      color = grDevices::rgb(0, 0, 0, 0.2)
    ) +
    ggnewscale::new_scale_fill() +
    geom_point(
      data = changepoint.dt,
      aes(
        x = date.numeric,
        y = get(var.name),
        fill = cp.deflection,
        shape = cp.deflection.sign
      ),
      size = 2.5,
      alpha = 0.85
    ) +
    effect.fill.scale +
    scale_shape_manual(values = c(25, 21, 24),
                       guide = "none")
  
  
  # Generate the density table, then gets scaled.
  generate.density.dt = function(density.dt)
    data.table(x = density(density.dt)$x,
               y = density(density.dt)$y)
  
  cp_1.density.dt = mcmc_post.dt[, generate.density.dt(cp_1), by = chain]
  
  y.range = ggplot_build(p)$layout$panel_scales_y[[1]]$range$range
  y.lims = ggplot_build(p)$layout$panel_scales_y[[1]]$limits
  density.min = ifelse(is.null(y.lims) ||
                         is.na(y.lims[1]), y.range[1], y.lims[1])
  density.max = ifelse(is.null(y.lims) ||
                         is.na(y.lims[2]), y.range[2], y.lims[2])
  if (is.null(density.height)) {
    density.height = density.max - density.min
  }
  cp_1.density.dt[, y.scaled := density.min + density.height * (y / max(y)), by = chain]
  
  p = p +
    geom_ribbon(
      data = cp_1.density.dt,
      aes(x = x, ymax = y.scaled, group = chain),
      alpha = 0.3,
      fill = '#000000',
      ymin = density.min
    ) +
    geom_line(data = cp_1.density.dt,
              aes(x = x, y = y.scaled, group = chain),
              linetype = 'dotted')
  
  return(p)
  
}
