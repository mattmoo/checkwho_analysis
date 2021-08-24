##' Draws a plot illustrating a changepoint analysis.
##'
##' @title
##' @param mcp.fit mcp changepoint model.
##' @param smooth.summary.dt Smoothed summary data.table (see generate.smooth.summary.dt).
##' @param density.height Height of the density plot in y units (default: half of limits).
##' @param period.rect.plot Optional ggplot item to highlight periods (Default: NULL).
##' @param y.scale Optional ggplot continuous scale for y axis (Default: NULL).
##' @param y.scale Optional number of lines (and hence changepoints) to illustrate (Default: 200).
draw.simpler.changepoint.plot <- function(mcp.fit,
                                  smooth.summary.dt,
                                  period.vline.plot = NULL,
                                  y.scale = NULL,
                                  x.time.scale = NULL,
                                  n.lines = 200,
                                  ribbon.transparency = 0.15,
                                  ribbon.colour = grDevices::rgb(0, 0, 0, ribbon.transparency),
                                  transform.function = NULL,
                                  prior = FALSE) {
  
  
  
  mcmc.dt = mcp.fit.to.mcmc.dt(mcp.fit, prior = prior)
  
  
  facet_by = NULL
  lines = n.lines
  geom_data = "point"
  cp_dens = TRUE
  q_fit = FALSE
  q_predict = FALSE
  rate = TRUE
  prior = prior
  which_y = "mu"
  arma = TRUE
  nsamples = 2000
  scale = "response"
  
  
  # Just for consistent naming in mcp
  fit = mcp.fit
  
  # Useful vars
  xvar = rlang::sym(fit$pars$x)
  yvar = rlang::sym(fit$pars$y)
  is_arma = length(fit$pars$arma) > 0
  
  
  ############################
  # MAKE NEWDATA AND PREDICT #
  ############################
  newdata = tibble::tibble(!!xvar := mcp:::get_eval_at(fit, facet_by))
  
  
  # Predict
  local_pp_eval = function(type) {
    mcp:::pp_eval(
      object = fit,
      newdata = newdata,
      summary = FALSE,  # Get samples
      type = type,
      rate = rate,
      prior = prior,
      which_y = which_y,
      varying = FALSE,
      arma = arma,
      nsamples = nsamples,
      samples_format = "tidy",
      scale = scale
    ) %>%
      dplyr::rename(!!yvar := !!type)  # from "predict"/"fitted" to yvar (response name)
  }
  
  
  lines.dt = as.data.table(local_pp_eval("fitted"))
  lines.dt = lines.dt[.draw %in% sample(unique(.draw), size = n.lines)]
  
  # Give them a super unique ID
  lines.dt[, iteration.id := paste0(.chain,'.',.iteration,'.',.draw)]
  
  # Mark line segments if you want to style them.
  lines.dt[, line.segment := 1]
  if ('cp_1' %in% names(lines.dt) & 'cp_2' %in% names(lines.dt)) {
    lines.dt[date.numeric >= cp_1 &
               date.numeric <= cp_2, line.segment := 2]
    lines.dt[date.numeric > cp_2,
             line.segment := 3]
  }
  
  
  changepoint.dt = lines.dt[line.segment==2, .SD[abs(cp_1 - date.numeric) == min(abs(cp_1 - date.numeric))], by = .draw]
  # changepoint.dt[, cp.deflection.2 := (date.numeric_2 - date.numeric_1) * (cp_2 - cp_1)]
  # changepoint.dt[, cp.deflection := transform.function((date.numeric_2 - date.numeric_1) * (cp_2 - cp_1))]
  changepoint.dt[, cp_2.projected := Intercept_1 + cp_2 * date.numeric_1]
  changepoint.dt[, cp_2.actual := Intercept_1 + cp_1 * date.numeric_1 + (cp_2-cp_1) * date.numeric_2]
  
  # if (!is.null(transform.function)) {
  #   changepoint.dt[, cp_2.projected := transform.function(cp_2.projected)]
  #   changepoint.dt[, cp_2.actual := transform.function(cp_2.actual)]
  # } else {
  #   changepoint.dt[, cp_2.projected := mcp.fit$family$linkinv(cp_2.projected)]
  #   changepoint.dt[, cp_2.actual := mcp.fit$family$linkinv(cp_2.actual)]
  # }
  
  changepoint.dt[, cp.deflection := cp_2.actual - cp_2.projected]
  changepoint.dt[, cp.deflection.sign := cp.deflection/abs(cp.deflection)]
  changepoint.dt[cp.deflection > -0.02 &
                   cp.deflection < 0.02, cp.deflection.sign := 0]
  changepoint.dt[, cp.deflection.sign := factor(cp.deflection.sign, levels = c(-1, 0, 1))]
  
  # changepoint.dt = changepoint.dt[order(cp.deflection)][1:5]
  # lines.dt = lines.dt[iteration.id %in% changepoint.dt[,iteration.id]]
  
  
  # Transform, this will break if these variables aren't present.
  if (!is.null(transform.function)) {
    set(x = lines.dt,
        j = mcp.fit$pars$y,
        value = lines.dt[, transform.function(get(mcp.fit$pars$y))])
    set(x = changepoint.dt,
        j = mcp.fit$pars$y,
        value = changepoint.dt[, transform.function(get(mcp.fit$pars$y))])
  } else {
    # set(x = lines.dt,
    #     j = mcp.fit$pars$y,
    #     value = lines.dt[, mcp.fit$family$linkinv(get(mcp.fit$pars$y))])
    # set(x = changepoint.dt,
    #     j = mcp.fit$pars$y,
    #     value = lines.dt[, mcp.fit$family$linkinv(get(mcp.fit$pars$y))])
  }
  
  # 
  # if (rev.fill.scale == TRUE) {
  #   effect.fill.scale = scale_fill_continuous_divergingx(
  #     name = 'Effect size',
  #     palette = 'RdBu',
  #     rev = TRUE,
  #     mid = 0,
  #     na.value = "black",
  #     labels = function(x) scales::percent(x,accuracy = 0.1)
  #   )
  # } else {
  #   effect.fill.scale = scale_fill_continuous_divergingx(
  #     name = 'Effect size',
  #     palette = 'RdBu',
  #     rev = FALSE,
  #     mid = 0,
  #     na.value = "black",
  #     labels = function(x) scales::percent(x,accuracy = 0.1)
  #   )
  # }
  
  smooth.summary.dt = smooth.summary.dt[!is.na(fit)]
  
  mcp.ribbon.dt = as.data.table(ggplot_build(plot(
    mcp.fit,
    q_fit = TRUE,
    q_predict = c(0.1, 0.9),
    geom_data = FALSE
  ))$data[[2]])
  
  
  p = ggplot() +
    geom_line(
      data = lines.dt,
      aes(group = iteration.id,
          x = date.numeric,
          y = !!yvar),
      color = grDevices::rgb(0, 0, 0, 0.1)
    ) +
    # geom_line(
    #   data = lines.dt[line.segment == 2],
    #   aes(group = iteration.id,
    #       x = date.numeric,
    #       y = !!yvar),
    #   color = grDevices::rgb(0, 0, 0, 0.2)
    # ) +
    period.vline.plot +
    y.scale +
    # ggnewscale::new_scale_fill() +
    # geom_point(
    #   data = changepoint.dt,
    #   aes(
    #     x = date.numeric,
    #     y = !!yvar,
    #     fill = cp.deflection,
    #     shape = cp.deflection.sign
    #   ),
    #   size = 2.5,
    #   alpha = 0.85
  # ) +
  # effect.fill.scale +
  # scale_shape_manual(values = c(25, 21, 24),
  #                    guide = "none") +
  geom_line(data = smooth.summary.dt,
            aes(x = as.numeric(time),
                y = fit),
            size = 0.75) +
    geom_ribbon(
      data = smooth.summary.dt,
      aes(x = time,
          ymin = ci.low,
          ymax = ci.high),
      fill = ribbon.colour,
      # colour = 'black',
      # linetype = 'dashed',
      colour = rgb(0,0,0,0.25)
      # linetype = NULL
    ) +
    
    geom_line(data = mcp.ribbon.dt,
              aes(x = x, y = y, group = group),
              colour = 'black',
              size = 0.75,
              linetype = 2
    ) +
    x.time.scale
  
  
  # # Generate the density table, then gets scaled.
  # generate.density.dt = function(density.dt)
  #   data.table(x = density(density.dt)$x,
  #              y = density(density.dt)$y)
  # 
  # cp_1.density.dt = mcmc.dt[, generate.density.dt(cp_1)]
  # 
  # y.range = ggplot_build(p)$layout$panel_scales_y[[1]]$range$range
  # y.lims = ggplot_build(p)$layout$panel_scales_y[[1]]$limits
  # density.min = ifelse(is.null(y.lims) ||
  #                        is.na(y.lims[1]), y.range[1], y.lims[1])
  # density.max = ifelse(is.null(y.lims) ||
  #                        is.na(y.lims[2]), y.range[2], y.lims[2])
  # if (is.null(density.height)) {
  #   density.height = density.max - density.min
  # }
  # # cp_1.density.dt[, y.scaled := density.min + density.height * (y / max(y)), by = chain]
  # cp_1.density.dt[, y.scaled := density.min + density.height * (y / max(y))]
  # 
  # 
  # p = p +
  #   geom_ribbon(
  #     data = cp_1.density.dt,
  #     aes(x = x, ymax = y.scaled),
  #     alpha = 0.3,
  #     fill = '#000000',
  #     ymin = density.min
  #   ) + x.time.scale
  
  return(p)
  
}
