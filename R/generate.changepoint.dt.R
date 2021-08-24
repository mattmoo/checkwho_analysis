##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param lines.dt
generate.changepoint.dt <- function(lines.dt,
                                    target.scaled.deflection.size = NULL,
                                    n.lines = changepoint.n.lines) {

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

  changepoint.dt[,cp.deflection.scaled := scale(cp.deflection)]
  changepoint.dt[,cp.deflection.scaled := cp.deflection.scaled/max(abs(cp.deflection.scaled))]
  
  changepoint.dt[, minus.target := abs(cp.deflection.scaled - target.scaled.deflection.size)]
  targeted.changepoint.dt = changepoint.dt[order(minus.target)][1:n.lines]
  
  if (!is.null(target.scaled.deflection.size)) {
    changepoint.dt[, minus.target := abs(cp.deflection.scaled - target.scaled.deflection.size)]
    changepoint.dt = changepoint.dt[order(minus.target)][1:n.lines]
  }
  
  return(changepoint.dt)
}
