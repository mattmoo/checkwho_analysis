##' Summary of binary variable over time.
##'
##' Calculates the proportion (with 95% confidence intervals) of a binary
##' variable over time. The variables should be logical.
##'
##' @title
##' @param data.dt Input data.table
##' @param time.col.name Name of the date/datetime column to be plotted on x
##'   axis.
##' @param measure.col.names Name of the continuous measures to summarise. They must
##'   be logical.
##' @param round.unit Time unit to round to (or a vector of the same), see
##'   lubridate::floor_date() (Default: '1 month').
##' @param conf.level Numeric confidence limits for confidence intervals (Default: 0.95)
##' @return Data.table with measure.col.names summarised in long format by
##'   requested time. Includes proportion, lwr.ci, and upr.ci.
generate.continuous.time.summary.dt <- function(data.dt,
                                                time.col.name,
                                                measure.col.names,
                                                round.unit = "1 month",
                                                conf.level = 0.95) {
  continuous.time.summary.dt = data.table()
  
  for (round.unit.it in round.unit) {
    # Get subset of input data
    input.dt = data.dt[,
                       c(time.col.name,
                         measure.col.names),
                       with = FALSE]
    
    # Round down time.
    input.dt[,
             time := floor_date(daoh.period.start,
                                unit = round.unit.it)]
    
    #Remove original time and transform data to long format table.
    input.dt = melt(
      input.dt[, c('time', measure.col.names), with = FALSE],
      id.vars = "time",
      measure.vars = measure.col.names,
      variable.name = "measure"
    )
    
    iteration.continuous.time.summary.dt = input.dt[,
                                                    .(mean = mean(value),
                                                      se = daohtools::std.err(value)), by = .(time, measure)]
    
    iteration.continuous.time.summary.dt[, ci.low := mean - se * qnorm(1 -
                                                                         (1 - conf.level) / 2)]
    iteration.continuous.time.summary.dt[, ci.high := mean + se * qnorm(1 -
                                                                          (1 - conf.level) / 2)]
    
    iteration.continuous.time.summary.dt[, round.unit := factor(round.unit.it)]
    
    continuous.time.summary.dt = rbindlist(list(
      continuous.time.summary.dt,
      iteration.continuous.time.summary.dt
    ))
    
  }
  
  
  return(continuous.time.summary.dt)
  
}
