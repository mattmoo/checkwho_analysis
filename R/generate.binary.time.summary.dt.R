##' Summary of binary variable over time.
##'
##' Calculates the proportion (with 95% confidence intervals) of a binary
##' variable over time. The variables should be logical.
##'
##' @title
##' @param data.dt Input data.table
##' @param time.col.name Name of the date/datetime column to be plotted on x
##'   axis.
##' @param measure.col.names Name of the binary measures to summarise. They must
##'   be logical.
##' @param round.unit Time unit to round to (or a vector of the same), see
##'   lubridate::floor_date() (Default: '1 month').
##' @param ci.method Method by which to calculate confidence intervals, see
##'   DescTools::BinomCI() (Default: "clopper-pearson").
##' @param conf.level Numeric confidence limits for confidence intervals (Default: 0.95)
##' @return Data.table with measure.col.names summarised in long format by
##'   requested time. Includes proportion, lwr.ci, and upr.ci.
generate.binary.time.summary.dt <- function(data.dt, 
                                            time.col.name,
                                            measure.col.names, 
                                            round.unit = "1 month", 
                                            ci.method = "clopper-pearson",
                                            conf.level = 0.95) {

  # Check columns are logical
  for (measure.col.name in measure.col.names) {
    if (class(data.dt[,get(measure.col.name)]) != 'logical') {
      warning(paste0('Column \'', measure.col.name, '\' is not logical, will not be summarised.'))
      measure.col.names = measure.col.names[measure.col.names != measure.col.name]
    }
  }
  
  binary.time.summary.dt = data.table()
  
  for(round.unit.it in round.unit) {
    
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
    input.dt = melt(input.dt[,c('time', measure.col.names), with = FALSE],
                    id.vars = "time",
                    measure.vars = measure.col.names,
                    variable.name = "measure")
    
    
    iteration.binary.time.summary.dt = input.dt[,
                                                cbind(as.data.table(
                                                  DescTools::BinomCI(
                                                    x = .N * mean(value),
                                                    n = .N,
                                                    conf.level = conf.level,
                                                    method = ci.method
                                                  )
                                                ),
                                                data.table(x = round(.N * mean(value)),
                                                           N = .N)), by = .(time, measure)]
    
    setnames(iteration.binary.time.summary.dt, old = 'est', new = 'proportion')
    
    iteration.binary.time.summary.dt[, round.unit := factor(round.unit.it)]
    
    binary.time.summary.dt = rbindlist(list(binary.time.summary.dt,
                                            iteration.binary.time.summary.dt))
  }
  
  
  return(binary.time.summary.dt)

}
