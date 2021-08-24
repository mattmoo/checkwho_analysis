##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param mort.los.risk.density.qlines.plot
##' @param risk.adjusted.regression.dt
##' @param dra.risk.quantiles.dt
generate.mort.los.risk.density.qlines.dots.plot <- function(mort.los.risk.density.qlines.plot,
                                                            risk.adjusted.regression.dt,
                                                            dra.risk.quantiles.dt,
                                                            shape.scale = c(1,25),
                                                            legend.position = 'none') {

  
  dra.risk.quantiles.dt[, mort.x := frollmean(dra.risk.quantiles.dt[,.(mort.value)], n = 2)]
  dra.risk.quantiles.dt[, los.y := frollmean(dra.risk.quantiles.dt[,.(los.value)], n = 2)]
  
  mort.pos.dt = dra.risk.quantiles.dt[!is.na(mort.x), .(
    riskgp = sprintf('%02dMORT', round(q * 10)),
    initial.pos = mort.x,
    final.pos = q * 6 - 3.3
  )]
  
  los.pos.dt = dra.risk.quantiles.dt[!is.na(mort.x), .(
    riskgp = sprintf('%02dLOS', round(q * 10)),
    initial.pos = los.y,
    final.pos = q * 6 - 3.3
  )]
  
  
  summary.dt = risk.adjusted.regression.dt[, .(N = .N, mean.daoh = mean(daoh)), by = .(SSC.riskgpMORT, SSC.riskgpLOS)]
  
  summary.dt = merge(summary.dt,
                     mort.pos.dt[, .(SSC.riskgpMORT = riskgp, x = initial.pos)],
                     by = 'SSC.riskgpMORT')
  summary.dt = merge(summary.dt,
                     los.pos.dt[, .(SSC.riskgpLOS = riskgp, y = initial.pos)],
                     by = 'SSC.riskgpLOS')
  
  p = mort.los.risk.density.qlines.plot +
    # geom_point(data = dra.risk.quantiles.dt, aes(x = mort.x, y = los.y), fill = 'white', colour = 'black', shape = 21)
    ggnewscale::new_scale_fill() +
    geom_point(data = summary.dt,
               mapping = aes(
                 x = x,
                 y = y,
                 size = N
               ), 
               pch = 21, colour = 'black',
               fill = 'white') + theme(legend.position = legend.position)
  
  return(p)

}
