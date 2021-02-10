##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param risk.adjusted.regression.dt
generate.dra.riskgp.group.diff.plot <- function(risk.adjusted.regression.dt) {

  summary.dt = risk.adjusted.regression.dt[!is.na(SSC), 
                                           .(N = .N, mean.daoh = mean(daoh)), 
                                           by = .(SSC, riskgpMORT, riskgpLOS, riskgp)]
  
  summary.dt[, prop := N/sum(N), by = SSC]
  
  diff.summary.dt = merge(x = summary.dt[SSC == 'Pre', .(riskgpMORT, riskgpLOS, N.pre = N, prop.pre = prop, mean.daoh)],
                          y = summary.dt[SSC == 'Post', .(riskgpMORT, riskgpLOS, N.post = N, prop.post = prop)])
  
  diff.summary.dt[, N.diff := N.post - N.pre]
  diff.summary.dt[, N.total := N.post + N.pre]
  diff.summary.dt[, prop.ratio := prop.post/prop.pre]
  diff.summary.dt[, prop.diff := prop.post - prop.pre]
  
  setorder(summary.dt, SSC, riskgp)
  
  # p = ggplot(data = diff.summary.dt,
  #             aes(x = riskgp,
  #                 y = N.diff,
  #                 fill = SSC)) + 
  #   geom_bar(stat = 'identity', position = position_dodge()) +
  #   theme(axis.text.x = element_text(angle = 90))
  # scale_fill_fermenter(breaks = seq(0,90,5), limits = c(40, 90), palette = 'Spectral', direction = 1)
  
  
  
  p = ggplot(
    diff.summary.dt,
    aes(
      x = forcats::fct_rev(riskgpMORT),
      y = forcats::fct_rev(riskgpLOS),
      size = N.total,
      fill = prop.ratio
    ),
    
  ) +
    geom_point(pch = 21, colour = 'black') +
    scale_fill_fermenter(
      breaks = seq(0.55, 1.45, 0.1),
      # breaks = seq(-0.01, 0.01, 0.002),
      # limits = c(40, 90),
      palette = 'Spectral',
      direction = 1,
      name = 'Ratio Post/Pre'
    ) +
    scale_size(range = c(0, 20))
  
  
  
  return(p)

}
