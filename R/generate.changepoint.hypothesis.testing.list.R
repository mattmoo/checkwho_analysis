##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param hypothesis A string hypothesis, as expected by mcp
##' @param changepoint.model.list A list of mcp models to compare against.
generate.changepoint.hypothesis.testing.list <- function(hypothesis,
                                                         changepoint.model.list) {

  changepoint.hypothesis.testing.list = list()
  for (model.ind in 1:length(changepoint.model.list)) {
    changepoint.hypothesis.testing.list[[names(changepoint.model.list)[model.ind]]] = 
      hypothesis(fit = changepoint.model.list[[model.ind]],
                 hypotheses = hypothesis)
  }
  
  return(changepoint.hypothesis.testing.list)

}
