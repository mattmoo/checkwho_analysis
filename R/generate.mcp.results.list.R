##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param bernoulli.mort.90.day.intervention.changepoint.mcp.fit
generate.mcp.results.list <- function(mcp.fit) {
  
  summary.dt = as.data.table(summary(mcp.fit))
  
  results.list = list(
    effect.per.year = list(
      pre = summary.dt[name == 'date.numeric_1', mean*365],
      pre.low = summary.dt[name == 'date.numeric_1', lower*365],
      pre.high = summary.dt[name == 'date.numeric_1', upper*365],
      post = summary.dt[name == 'date.numeric_2', mean*365],
      post.low = summary.dt[name == 'date.numeric_2', lower*365],
      post.high = summary.dt[name == 'date.numeric_2', upper*365],
      p = hypothesis(mcp.fit, 'date.numeric_2 > date.numeric_1')
    ),
    changepoint.location = list(
      mean = summary.dt[name == 'cp_1', as.Date(mean)],
      ci.low = summary.dt[name == 'cp_1', as.Date(lower)],
      ci.high = summary.dt[name == 'cp_1', as.Date(upper)]
    ),
    magnitude = hypothesis(mcp.fit, '((date.numeric_2 - date.numeric_1) * (cp_2 - cp_1)) > 0')
  )
  
  return(results.list)

}
