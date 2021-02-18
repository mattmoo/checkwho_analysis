##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param input.dt
##' @param daoh.risk.adjust.model
##' @param mort.90.risk.adjust.model
##' @param mort.30.risk.adjust.model
##' @param dra.risk.adjust.model.list 
risk.adjust.regression.dt <- function(input.dt, 
                                      daoh.risk.adjust.model,
                                      mort.90.risk.adjust.model,
                                      mort.30.risk.adjust.model,
                                      SSC.dra.risk.adjust.model.list,
                                      ethnicity.dra.risk.adjust.model.list) {

  risk.adjusted.input.dt = copy(input.dt)
  
  # Risk adjust DAOH with quantile regression
  risk.adjusted.input.dt = merge(
    x = risk.adjusted.input.dt,
    y = daohtools::risk.adjust.on.quantreg.model(risk.adjusted.input.dt,
                                                 daoh.risk.adjust.model,
                                                 by.reference = FALSE)[
                                                   , .(index.event.id, daoh.risk.adj)],
    by = 'index.event.id'
  )
  
  # Risk adjust mortality.
  risk.adjusted.input.dt[, mort.90.day.predicted := predict(mort.90.risk.adjust.model,
                                                            newdata = input.dt,
                                                            type = "response")]
  risk.adjusted.input.dt[, mort.90.day.risk.adj := mort.90.day / mort.90.day.predicted * mean(mort.90.day)]
  
  risk.adjusted.input.dt[, mort.30.day.predicted := predict(mort.30.risk.adjust.model,
                                                            newdata = input.dt,
                                                            type = "response")]
  risk.adjusted.input.dt[, mort.30.day.risk.adj := mort.30.day / mort.30.day.predicted * mean(mort.30.day)]
  
  # Add weights for risk adjusting DAOH with direct risk adjustment.
  generate.dra.riskgp.columns(risk.adjusted.input.dt,
                              model.list = SSC.dra.risk.adjust.model.list,
                              col.prefix.name = 'SSC.')
  
  risk.adjusted.input.dt = calculate.dra.weights(
    input.dt = risk.adjusted.input.dt,
    reference.level = NULL,
    group.col.name = 'SSC',
    weight.col.name = 'SSC.dra.weight',
    riskgp.col.name = 'SSC.riskgp'
  )
  
  generate.dra.riskgp.columns(risk.adjusted.input.dt,
                              model.list = ethnicity.dra.risk.adjust.model.list,
                              col.prefix.name = 'ethnicity.')
  
  risk.adjusted.input.dt = calculate.dra.weights(
    input.dt = risk.adjusted.input.dt,
    group.col.name = 'ethnicity',
    weight.col.name = 'ethnicity.dra.weight',
    riskgp.col.name = 'ethnicity.riskgp',
    reference.level = 'Maori'
  )
  
  setorder(risk.adjusted.input.dt, index.event.id)
  
  return(risk.adjusted.input.dt)
  
}
