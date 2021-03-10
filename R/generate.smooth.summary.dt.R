##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data.dt
##' @param time.col.name
##' @param measure.col.names
##' @param smooth.duration
##' @param conf.level
generate.smooth.summary.dt <- function(data.dt = time.series.figure.dt,
                                       time.col.name = "daoh.period.start",
                                       measure.col.names = c("daoh",
                                       "mort.90.day", "mort.30.day"),
                                       smooth.duration = c("3 months", "6
                                       months", "1 year"), conf.level = 0.95) {

  
  
  smooth.dt = data.table()
  
  setorderv(data.dt, time.col.name)
  
  loess.smooth.dt = function(input.dt,
                             smooth.duration,
                             conf.level = 0.95,
                             sampling.freq = '1 week',
                             time.unit = days(1)) {
    
    
    # Generate span.
    span.days = as.period(as.duration(smooth.duration)) / time.unit
    date.range = range(input.dt[,time])
    total.days = as.numeric(diff(date.range))
    
    # Fit the model.
    fit.loess = loess(value ~ as.numeric(time), 
                      span = span.days / total.days, 
                      data = input.dt,
                      degree = 1)
    
    # Generate list of days.
    smooth.dt = data.table(time = seq.Date(from = date.range[1],
                                           to = date.range[2],
                                           by = '1 day'))
    smooth.dt = unique(smooth.dt[, time := floor_date(time, unit = sampling.freq)])
    
    
    # Get a list of smooth parameters.
    smooth.dt = cbind(smooth.dt,
                      as.data.table(predict(fit.loess, newdata = smooth.dt, se = TRUE)))
    smooth.dt[, ci.low := fit - se.fit * qt(1 - (1 - conf.level) / 2, df)]
    smooth.dt[, ci.high := fit + se.fit * qt(1 - (1 - conf.level) / 2, df)]
    # These columns aren't needed
    smooth.dt[, df := NULL]
    smooth.dt[, residual.scale := NULL]
    # Put in the smooth duration
    smooth.dt[, smooth.duration := smooth.duration]
    
    return(smooth.dt)
    
  }
  
  
  #
  for (smooth.duration.it in smooth.duration) {
    # Get subset of input data
    input.dt = data.dt[,
                       c(time.col.name,
                         measure.col.names),
                       with = FALSE]
    
    setnames(input.dt, old = time.col.name, new = 'time')
    
    #Remove original time and transform data to long format table.
    input.dt = melt(
      input.dt[, c('time', measure.col.names), with = FALSE],
      id.vars = "time",
      measure.vars = measure.col.names,
      variable.name = "measure"
    )
    
    smooth.iteration.dt = input.dt[,
                                   loess.smooth.dt(input.dt = .SD,
                                                   smooth.duration = smooth.duration.it),
                                   by = .(measure)]
    
    
    
    smooth.dt = rbindlist(
      list(
        smooth.dt,
        smooth.iteration.dt
      )
    )
  }
  
  return(smooth.dt)
  

}
