##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param risk.adjusted.regression.dt
draw.dra.riskgp.group.diff.plot <-
  function(risk.adjusted.regression.dt,
           summary.col.name,
           riskgp.col.names,
           riskgp.axis.names = NULL,
           riskgp.direction = 1,
           shape.scale = c(1, 25)) {
    
  
  if (is.null(riskgp.axis.names)) {
    riskgp.axis.names = riskgp.col.names
  }
  
  riskgp.direction = 1
  circle.scale = c(1, 25)
  
  
  summary.levels = levels(risk.adjusted.regression.dt[,get(summary.col.name)])
  if (length(summary.levels) != 2) {
    stop(paste(summary.col.name, 'has more than two levels.'))
  }
  
  if (length(riskgp.col.names) != 2) {
    stop(paste('riskgp.col.names has length that is not two.'))
  }
  
  # Summarise by level of variable of interest in long format.
  summary.dt = risk.adjusted.regression.dt[!is.na(get(summary.col.name)), 
                                           .(N = .N, mean.daoh = mean(daoh)), 
                                           by = c(summary.col.name, riskgp.col.names)]
  summary.dt[, prop := N/sum(N), by = summary.col.name]
  setorderv(summary.dt, c(riskgp.col.names, summary.col.name))
  
  # Make it wider, and calculate differences.
  dcast.formula = as.formula(paste(paste0(riskgp.col.names, collapse = '+'), '~', summary.col.name))
  diff.summary.dt = dcast(data = summary.dt,
                          formula = dcast.formula,
                          value.var = c('prop', 'N'),
                          fill = 0)
  
  
  diff.summary.dt[, N.diff := get(paste0('N_', summary.levels[1])) - get(paste0('N_', summary.levels[2]))]
  diff.summary.dt[, N.total := get(paste0('N_', summary.levels[1])) + get(paste0('N_', summary.levels[2]))]
  diff.summary.dt[, prop.ratio := get(paste0('prop_', summary.levels[1])) / get(paste0('prop_', summary.levels[2]))]
  diff.summary.dt[, prop.diff := get(paste0('prop_', summary.levels[1])) - get(paste0('prop_', summary.levels[2]))]
  setorderv(summary.dt, riskgp.col.names)
  
  
  # Derive some columns for graphing.
  diff.summary.dt[, log.prop.ratio := log(prop.ratio)]
  diff.summary.dt[, log.prop.ratio.sign := log.prop.ratio/abs(log.prop.ratio)]
  diff.summary.dt[log.prop.ratio > -0.05 & log.prop.ratio < 0.05, log.prop.ratio.sign := 0]
  diff.summary.dt[is.infinite(log.prop.ratio), log.prop.ratio.sign := 0]
  diff.summary.dt[, log.prop.ratio.sign := factor(log.prop.ratio.sign, levels = c(-1, 0, 1))]
  
  # Chuck mean DAOH in there 
  diff.summary.dt = merge(x = diff.summary.dt,
                          y = risk.adjusted.regression.dt[, .(
                            mean.daoh = mean(daoh)
                          ), by = riskgp.col.names],
                          by = riskgp.col.names)
  
  # # Reverse factors if requested.
  # if (reverse.riskgp.factors) {
  #   for (riskgp.col.name in riskgp.col.names) {
  #     set(x = diff.summary.dt,
  #         j = riskgp.col.name,
  #         value = forcats::fct_rev(diff.summary.dt[,get(riskgp.col.name)]))
  #   }
  # }
  
  x.y.labels = rep("", length(summary.dt[, unique(get(riskgp.col.names[1]))]))
  x.y.labels[1] = "Lowest"
  x.y.labels[length(x.y.labels)] = "Highest"
  if (riskgp.direction == -1) {
    x.y.labels = rev(x.y.labels)
  }
  
  colour.bar.label = paste0('Log ratio', '\n', summary.levels[1], '/', summary.levels[2])
  
  p = ggplot(
    diff.summary.dt,
    aes(
      x = get(riskgp.col.names[1]),
      y = get(riskgp.col.names[2]),
      size = N.total,
      fill = log.prop.ratio,
      shape = log.prop.ratio.sign
    )
  ) +
    # geom_raster(
    #   aes(fill = mean.daoh),
    #   interpolate = TRUE,
    #   alpha = 0.35
    # ) +
    geom_point(
      aes(size = N.total,
          fill = log.prop.ratio,
          shape = log.prop.ratio.sign
    )) +
    
    scale_fill_continuous_divergingx(
      palette = 'RdBu',
      mid = 0,
      na.value = "black"
    ) +
    scale_size(range = circle.scale, name = "N (total)") +
    scale_shape_manual(values = c(25, 21, 24)) + 
    scale_x_discrete(labels = x.y.labels, name = riskgp.axis.names[1]) + 
    scale_y_discrete(labels = x.y.labels, name = riskgp.axis.names[2]) +
    guides(shape = FALSE, size = FALSE) +
    labs(fill = colour.bar.label)
  
  return(p)

}
