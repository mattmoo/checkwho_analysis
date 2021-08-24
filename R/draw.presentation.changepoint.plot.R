##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param mcp.fit
##' @param smooth.summary.dt
##' @param lines.dt
##' @param changepoint.dt
##' @param draw.points
##' @param draw.period
##' @param draw.smooth.line
##' @param draw.lines
##' @param draw.triangle
##' @param target.scaled.deflection.size.vector
##' @param period.rect.plot
##' @param period.rect.scale
##' @param y.scale
##' @param x.time.scale
##' @param n.lines
##' @param ribbon.transparency
##' @param ribbon.colour
##' @param prior
draw.presentation.changepoint.plot <-
  function(mcp.fit,
           smooth.summary.dt,
           lines.dt,
           changepoint.dt,
           draw.points = FALSE,
           draw.period = TRUE,
           draw.smooth.line = TRUE,
           draw.lines = TRUE,
           draw.triangle = TRUE,
           draw.fit.ribbon = FALSE,
           # target.scaled.deflection.size.vector = 0.4,
           period.rect.plot = NULL,
           period.rect.scale = NULL,
           # y.scale = NULL,
           x.time.scale = NULL,
           n.lines = 200,
           ribbon.transparency = 0.15,
           ribbon.colour = grDevices::rgb(0, 0, 0, ribbon.transparency),
           prior = FALSE,
           only.earlier.triangle = FALSE) {
    
    # if (!is.null(target.scaled.deflection.size.vector)) {
    #   changepoint.dt = rbindlist(
    #     lapply(target.scaled.deflection.size.vector,
    #            function(x)
    #              generate.changepoint.dt(lines.dt,
    #                                      x,
    #                                      n.lines = 1))
    #   )
    #   lines.dt = lines.dt[iteration.id %in% changepoint.dt[, iteration.id]]
    # }
    
    # changepoint.dt = changepoint.dt[sample(1:.N, min(.N, n.lines))]
    
    
    xvar = rlang::sym(mcp.fit$pars$x)
    yvar = rlang::sym(mcp.fit$pars$y)
    
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
    
    
    if (draw.points) {
      y.scale = scale_y_continuous(name = "Days alive and out of hospital",
                                   # limits = c(72, NA),
                                   breaks = seq(0, 90, by = 15))
    } else {
      y.scale = scale_y_continuous(
        name = "Days alive and out of hospital",
        limits = c(72, NA),
        breaks = seq(0, 90, by = 1)
      )
    }
    
    # id.dt = changepoint.dt[, .(iteration.id = unique(iteration.id))][, new.id := .I]
    # lines.dt = merge(x = lines.dt,
    #                  y = id.dt)
    # changepoint.dt = merge(x = changepoint.dt,
    #                        y = id.dt)
    
    # changepoint.dt[, iteration.id := .draw]
    # lines.dt[, iteration.id := .draw]
    
    
    # # Transform, this will break if these variables aren't present.
    # if (!is.null(transform.function)) {
    #   set(x = lines.dt,
    #       j = mcp.fit$pars$y,
    #       value = lines.dt[, transform.function(get(mcp.fit$pars$y))])
    #   set(x = changepoint.dt,
    #       j = mcp.fit$pars$y,
    #       value = changepoint.dt[, transform.function(get(mcp.fit$pars$y))])
    # } else {
    #   
    # }
    
    
    smooth.summary.dt = smooth.summary.dt[!is.na(fit)]
    
    
    p = ggplot() +
      y.scale +
      x.time.scale +
      theme(legend.position = 'none')
    
    
    if (draw.points) {
      p = p +
        geom_point(data = time.series.figure.dt,
                   aes(x = daoh.period.start,
                       y = daoh),
                   alpha = 0.4)
    }
    
    
    
    if (draw.period) {
      p = p +
        period.rect.scale +
        period.rect.plot
    }
    
    ribbon.alpha = 0.8
    if (draw.lines) {
      p = p +
        geom_line(
          data = lines.dt,
          aes(
            group = new.id,
            x = date.numeric,
            y = !!yvar
          ),
          alpha = lines.dt$alpha,
          color = grDevices::rgb(0, 0, 0, 0.8)
        )
      ribbon.alpha = 0.4
    }
    if (draw.fit.ribbon) {
      
      
      mcp.ribbon.dt = as.data.table(ggplot_build(plot(
        mcp.fit,
        q_fit = TRUE,
        q_predict = c(0.1, 0.9),
        geom_data = FALSE
      ))$data[[2]])
      
      p = p +
        geom_line(data = mcp.ribbon.dt,
                  aes(x = x, y = y, group = group),
                  colour = 'black',
                  size = 0.75,
                  linetype = 2)
      ribbon.alpha = 0.4
      
    }
    
    
    if (draw.smooth.line) {
      p = p +
        geom_line(
          data = smooth.summary.dt,
          aes(x = as.numeric(time),
              y = fit),
          size = 1,
          alpha = ribbon.alpha
        ) +
        geom_ribbon(
          data = smooth.summary.dt,
          aes(x = time,
              ymin = ci.low,
              ymax = ci.high),
          fill = ribbon.colour,
          colour = 'black',
          linetype = 'dashed'
        )
    }
    
    if (draw.triangle) {
      shape.dt =
        changepoint.dt[, .(
          iteration.id,
          x = c(cp_1, cp_2, cp_2),
          y = c(daoh, cp_2.projected, cp_2.actual),
          cp.deflection = cp.deflection,
          cp.deflection.scaled = cp.deflection.scaled,
          cp.deflection.sign = cp.deflection,
          alpha = alpha
        )]
      
      if (only.earlier.triangle) {
        shape.dt = shape.dt[iteration.id != max(iteration.id)]
      }
      
      p = p +
        ggnewscale::new_scale_fill() +
        geom_polygon(
          data = shape.dt,
          aes(
            x = x,
            y = y,
            fill = cp.deflection.scaled,
            group = iteration.id
          ),
          alpha = shape.dt$alpha
        ) +
        # geom_line(data = shape.dt,
        #           aes(x = x,
        #               y = y,
        #               group = iteration.id),
        #           alpha = shape.dt$alpha) +
        scale_fill_continuous_divergingx(
          palette = 'Spectral',
          mid = 0,
          na.value = "black",
          limits = c(-0.85, 0.85),
          labels = function(x)
            sprintf("%+.2f", x)
        )
      
    }
    

    
    return(p)
    
    
  }
