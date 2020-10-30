##' Summary of a numeric variable over time using quantiles.
##'
##' Calculates the quantile (with 95% confidence intervals) of a binary
##' variable over time. The variables should be numeric or double.
##'
##' @title
##' @param data.dt Input data.table
##' @param time.col.name Name of the date/datetime column to be plotted on x
##'   axis.
##' @param measure.col.names Name of the binary measures to summarise. They must
##'   be logical.
##' @param probs Quantile or quantiles to assess at (Default: 0.5)
##' @param round.unit Time unit to round to, see lubridate::floor_date()
##'   (Default: '1 month').
##' @return Data.table with measure.col.names summarised in long format by
##'   requested time, and quantile. Includes prob (i.e. quantile), value,
##'   lwr.ci, and upr.ci.
generate.quantile.time.summary.dt <- function(data.dt,
                                              time.col.name,
                                              measure.col.names,
                                              probs = c(0.5),
                                              round.unit = "1 month") {
  
  # Check columns are numeric
  for (measure.col.name in measure.col.names) {
    if (!(is.double(data.dt[, get(measure.col.name)]) |
          is.numeric(data.dt[, get(measure.col.name)]))) {
      warning(
        paste0(
          'Column \'',
          measure.col.name,
          '\' is not numeric (or double), will not be summarised.'
        )
      )
      measure.col.names = measure.col.names[measure.col.names != measure.col.name]
    }
  }
  
  # Helper function to wrap quantileCI similar to DescTools::BinomCI
  quantile.plus.confint = function(x, prob = 0.5) {
    ci = quantileCI::quantile_confint_exact(x = x,
                                            p = prob,
                                            conf.level = 0.95)
    result = list(
      value = quantile(x, probs = prob),
      lwr.ci = ci[1],
      upr.ci = ci[2]
    )
    
    return(result)
  }
  
  # Get subset of input data.
  input.dt = data.dt[,
                     c(time.col.name,
                       measure.col.names),
                     with = FALSE]
  
  # Round down time.
  input.dt[,
           time := floor_date(daoh.period.start,
                              unit = round.unit)]
  
  # Remove original time and transform to long format.
  input.dt = melt(
    input.dt[, c('time', measure.col.names), with = FALSE],
    id.vars = "time",
    measure.vars = measure.col.names,
    variable.name = "measure"
  )
  
  # Helper function to summarise for one quantile.
  generate.quantile.dt = function(prob) {
    input.dt[,
             c(prob = prob,
               quantile.plus.confint(x = value,
                                     prob = prob)), by = .(time, measure)]
  }
  # Generate all requested quantiles.
  summary.dt = rbindlist(lapply(probs, generate.quantile.dt))
  
  summary.dt[, prob := factor(prob)]
  
  return(summary.dt)

}
