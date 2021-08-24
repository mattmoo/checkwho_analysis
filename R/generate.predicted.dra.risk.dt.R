##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param time.series.figure.dt
generate.predicted.dra.risk.dt <- function(time.series.figure.dt,
                                           dra.risk.adjust.model.list) {

  predicted.dra.risk.dt = time.series.figure.dt[, .(
    mort.pred = predict(dra.risk.adjust.model.list$MORT, newdata = time.series.figure.dt),
    los.pred = predict(dra.risk.adjust.model.list$LOS, newdata = time.series.figure.dt))]

  predicted.dra.risk.dt[, scaled.mort := scale(mort.pred)]
  predicted.dra.risk.dt[, scaled.los := scale(los.pred)]
  
  return(predicted.dra.risk.dt)
}
