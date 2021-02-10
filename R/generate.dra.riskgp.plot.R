##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param risk.adjusted.regression.dt
generate.dra.riskgp.plot <- function(risk.adjusted.regression.dt) {

  
  summary.dt = risk.adjusted.regression.dt[, .(N = .N, mean.daoh = mean(daoh)), by = .(riskgpMORT, riskgpLOS)]
  
  
  dra.riskgp.plot = ggplot(summary.dt, aes(x = forcats::fct_rev(riskgpMORT), y = forcats::fct_rev(riskgpLOS), size = N, fill = mean.daoh), ) + 
    geom_point(pch=21, colour = 'black') + 
    scale_fill_fermenter(breaks = seq(0,90,5), limits = c(40, 90), palette = 'Spectral', direction = 1) +
    scale_size(range = c(0, 30))
  
  
  
  return(dra.riskgp.plot)

}
