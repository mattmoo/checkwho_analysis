##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param risk.adjusted.regression.dt
generate.dra.riskgp.daoh.animation <- function(input.dt) {

  
  input.dt[, riskgp.numeric := as.numeric(str_extract(riskgpMORT, '\\d+')) + as.numeric(str_extract(riskgpLOS, '\\d+'))]
  nb.cols = length(unique(input.dt[,riskgp.numeric]))
  
  
  # input.dt = input.dt[riskgp.numeric > 15]
  
  input.summary.dt = merge(x = data.table(expand.grid(daoh = 0:90, riskgp.numeric = (unique(input.dt[,riskgp.numeric])))),
                           y = calculate.summary.dt(input.dt, by.group = 'riskgp.numeric'),
                           by = c('daoh', 'riskgp.numeric'),
                           all.x = TRUE)
  input.summary.dt[is.na(N), N := 0]
  input.summary.dt[is.na(prop), prop := 0]
  
  input.summary.dt[, riskgp.numeric.factor := forcats::fct_rev(ordered(riskgp.numeric))]
  
  p = ggplot2::ggplot(input.summary.dt, ggplot2::aes(
    x = daoh, 
    y = prop,
    # y = after_stat(density),
    fill = riskgp.numeric
  )) +
    geom_bar(width = 1, stat = "identity", position = "identity", colour = 'black')
  
  
  p = p +
    ggplot2::labs(title="Frequency for Overall DAOH") +
    ggplot2::labs(x="Days alive and out of hospital", y="Proportion") +
    x.scale +
    transformed.y.scale +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    scale_fill_fermenter_custom(pal = colorRampPalette(brewer.pal(11, "Spectral"))(nb.cols), breaks = 20:2, limits = c(2, 20))
  # scale_fill_manual(values = colorRampPalette(brewer.pal(11, "Spectral"))(nb.cols))
  
  a = p +
    transition_states(riskgp.numeric,
                      transition_length = 2,
                      state_length = 5, 
                      wrap = FALSE) +
    ease_aes('linear')
  
  
  return(a)

}
