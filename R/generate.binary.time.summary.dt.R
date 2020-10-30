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
##' @param round.unit Time unit to round to, see lubridate::floor_date()
##'   (Default: '1 month').
##' @param ci.method Method by which to calculate confidence intervals, see
##'   DescTools::BinomCI() (Default: "clopper-pearson").
##' @return Data.table with measure.col.names summarised in long format by
##'   requested time. Includes proportion, lwr.ci, and upr.ci.
generate.binary.time.summary.dt <- function(data.dt, 
                                            time.col.name,
                                            measure.col.names, 
                                            round.unit = "1 month", 
                                            ci.method = "clopper-pearson") {

  
  # Check columns are logical
  for (measure.col.name in measure.col.names) {
    if (class(data.dt[,get(measure.col.name)]) != 'logical') {
      warning(paste0('Column \'', measure.col.name, '\' is not logical, will not be summarised.'))
      measure.col.names = measure.col.names[measure.col.names != measure.col.name]
    }
  }
  
  # Get subset of input data
  input.dt = data.dt[, 
                     c(time.col.name, 
                       measure.col.names),
                     with = FALSE]
  # Round down time.
  input.dt[,
           time := floor_date(daoh.period.start, 
                              unit = round.unit)]
  
  #Remove original time and transform data to long format table.
  input.dt = melt(input.dt[,c('time', measure.col.names), with = FALSE],
                  id.vars = "time",
                  measure.vars = measure.col.names,
                  variable.name = "measure")
  
  
  binary.time.summary.dt = input.dt[,
                                    as.data.table(DescTools::BinomCI(
                                      x = .N * mean(value),
                                      n = .N,
                                      conf.level = 0.95,
                                      method = ci.method
                                    )), by = .(time, measure)]
  
  setnames(binary.time.summary.dt, old = 'est', new = 'proportion')
  
  return(binary.time.summary.dt)

}
