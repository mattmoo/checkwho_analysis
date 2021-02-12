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
  
  
  x.breaks =  seq(-600, 600, by = 15)
  x.scale = ggplot2::scale_x_continuous(breaks = x.breaks,
                                        expand = c(0.01, 0.1),
                                        limits = c(-0.5, 90.5))
  
  y.scale.labels = function(x)
    sprintf("%0.2g%%", round(x * 100, digits = 5))
  ybreaks =  c(0,
               0.001,
               0.0025,
               0.005,
               0.01,
               0.02,
               0.03,
               0.04,
               0.05,
               0.075,
               seq(from = 0.1, to = 1, by = 0.05))
  
  transformed.y.scale = scale_y_sqrt(
    labels = y.scale.labels,
    minor_breaks = NULL,
    breaks = ybreaks,
    expand = c(0.001, 0.001),
    limits = c(0, .45)
  )
  
  # https://stackoverflow.com/questions/64013935/custom-color-palette-for-scale-fill-fermenter
  scale_fill_fermenter_custom <- function(pal, na.value = "grey50", guide = "coloursteps", aesthetics = "fill", ...) {
    binned_scale("fill", "fermenter", ggplot2:::binned_pal(scales::manual_pal(unname(pal))), na.value = na.value, guide = guide, ...)  
  }
  
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
