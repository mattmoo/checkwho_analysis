##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param input.dt
##' @param daoh.risk.adjust.model
##' @param mort.90.risk.adjust.model
##' @param mort.30.risk.adjust.model
risk.adjust.regression.dt <- function(input.dt, 
                                      daoh.risk.adjust.model,
                                      mort.90.risk.adjust.model,
                                      mort.30.risk.adjust.model) {

  risk.adjusted.input.dt = copy(input.dt)
  
  risk.adjusted.input.dt = merge(
    x = risk.adjusted.input.dt,
    y = daohtools::risk.adjust.on.quantreg.model(risk.adjusted.input.dt,
                                                 daoh.risk.adjust.model,
                                                 by.reference = FALSE)[
                                                   , .(index.event.id, daoh.risk.adj)],
    by = 'index.event.id'
  )
  
 
  risk.adjusted.input.dt[, mort.90.day.predicted := predict(mort.90.risk.adjust.model,
                                                            newdata = input.dt,
                                                            type = "response")]
  risk.adjusted.input.dt[, mort.90.day.risk.adj := mort.90.day / mort.90.day.predicted * mean(mort.90.day)]
  
  risk.adjusted.input.dt[, mort.30.day.predicted := predict(mort.30.risk.adjust.model,
                                                            newdata = input.dt,
                                                            type = "response")]
  risk.adjusted.input.dt[, mort.30.day.risk.adj := mort.30.day / mort.30.day.predicted * mean(mort.30.day)]
  
  
  
  
  return(risk.adjusted.input.dt)
  
}
