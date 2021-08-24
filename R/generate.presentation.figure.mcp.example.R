##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

generate.presentation.figure.mcp.example <- function() {

  model = list(
    y ~ 1,
    ~ 0 + x + I(x^2)
  )
  ex = mcp_example("quadratic")
  fit = mcp(model, ex$data)
  
  p = plot(fit, cp_dens = F, lines = 1)
  
  return(p)

}
