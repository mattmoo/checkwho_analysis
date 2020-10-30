##' Generate a list of mcp models for the various binary measures.
##'
##' @title
##' @param time.series.figure.dt Data.table with only eligible cases, and all
##'   columns specified in changepoint.measure.list
##' @param changepoint.measure.list List of binary variables to measure
##'   changepoints on.
##' @param changepoint.prior.list List of priors for mcp
##' @param changepoint.model.func Fucntion that takes a measure name
##'   (character), and returns a list of formulae for mcp
##' @param ... Other arguments for mcp
generate.changepoint.model.list <- function(time.series.figure.dt,
                                            changepoint.measure.list,
                                            ssc.implementation.start,
                                            ssc.implementation.end,
                                            changepoint.prior.list = NULL, 
                                            changepoint.model.func = NULL,
                                            ...) {

  changepoint.model.list = list()
  
  for (measure.ind in 1:length(changepoint.measure.list)) {
    
    measure = changepoint.measure.list[[measure.ind]]
    
    if (is.null(changepoint.prior.list)) {
      prior = list()
    } else {
      prior = changepoint.prior.list[[measure.ind]]
    }
    
    # Generate summary table
    binomial.figure.dt = time.series.figure.dt[,.(N = .N,
                                                  sum(get(measure))),
                                               by = .(month = floor_date(daoh.period.start, unit = '1 months'))]
    setnames(binomial.figure.dt, 'V2', measure)
    
    binomial.figure.dt[, month.numeric := as.numeric(month)]
    start.month = binomial.figure.dt[,min(month)]
    end.month = binomial.figure.dt[,max(month)]
    
    if (is.null(changepoint.model.func)) {
      # Generate model with two change points
      model = list(
        as.formula(paste0(measure, ' | trials(N) ~ 1 + month.numeric')),
        ~ 0 + month.numeric,
        ~ 0 + month.numeric
      )
    } else {
      model = changepoint.model.func(measure)
    }
    
    print(model)
    
    fit = mcp::mcp(
      model,
      data = binomial.figure.dt,
      prior = prior,
      family = binomial(),
      sample = "both",
      ...
    )
    
    
    changepoint.model.list[[measure]] = fit
    
  }
  
  return(changepoint.model.list)

}
