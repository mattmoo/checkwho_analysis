##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param risk.adjusted.regression.dt
generate.dra.riskgp.plot <- function(risk.adjusted.regression.dt,
                                     riskgp.direction = 1,
                                     shape.scale = c(1, 25),
                                     riskgp.axis.names) {
  
  
  
  summary.dt = risk.adjusted.regression.dt[, .(N = .N, mean.daoh = mean(daoh)), by = .(SSC.riskgpMORT, SSC.riskgpLOS)]
  
  x.y.labels = rep("", length(summary.dt[, unique(SSC.riskgpMORT)]))
  x.y.labels[1] = "Lowest"
  x.y.labels[length(x.y.labels)] = "Highest"
  
  if (riskgp.direction == -1) {
    x.y.labels = rev(x.y.labels)
  }
  
  dra.riskgp.plot = ggplot(summary.dt,
                           aes(
                             x = SSC.riskgpMORT,
                             y = SSC.riskgpLOS,
                             size = N,
                             fill = mean.daoh
                           )) +
    geom_point(pch = 21, colour = 'black') +
    scale_fill_fermenter(
      breaks = seq(0, 90, 5),
      limits = c(40, 90),
      palette = 'Spectral',
      direction = 1
    ) +
    scale_x_discrete(labels = x.y.labels, name = riskgp.axis.names[1]) +
    scale_y_discrete(labels = x.y.labels, name = riskgp.axis.names[2]) +
    scale_size(range = shape.scale)
  
  
  
  return(dra.riskgp.plot)
  
}
