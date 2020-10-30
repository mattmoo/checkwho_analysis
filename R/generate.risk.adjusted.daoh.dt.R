##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param regression.dt Data.table with DAOH, and columns on which to risk
##'   adjust.
generate.risk.adjusted.daoh.dt <- function(regression.dt) {
  
  risk.adjusted.daoh.dt = daohtools::risk.adjust.quantreg(
    input.dt = regression.dt,
    output.variable,
    covariates,
    jitter.output.variable = FALSE,
    quantiles.to.assess = seq(0.25, 0.75, by = 0.1),
    by.reference = FALSE
  )

}
