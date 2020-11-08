##' Make a table from which number can be copied into a flow figure.
##'
##' @title
##' @param eligibility.dt
##' @param eligibility.factor.vector List of factors
generate.eligibility.figure.numbers.dt <- function(eligibility.dt,
                                                   eligibility.factor.vector) {

  
  # eligibility.dt = eligibility.dt[(after.min.date & before.max.date) | is.na(after.min.date) == TRUE]
  
  assess.inclusion.criterion = function(eligibility.factor.name) {
    
    result.row.dt = data.table(inclusion.criterion = eligibility.factor.name,
                               n = eligibility.dt[ALL.PREV &
                                                    get(eligibility.factor.name) == TRUE, .N])
    
    eligibility.dt[, ALL.PREV := ALL.PREV & get(eligibility.factor.name)]
    
    return (result.row.dt)
  }
  
  assess.inclusion.criteria = function(eligibility.factor.vector) {
    rbindlist(lapply(X = eligibility.factor.vector, FUN = assess.inclusion.criterion))
  }
  
  eligibility.dt[, ALL.PREV := TRUE]
  
  eligibility.figure.numbers.dt = rbind(
    data.table(inclusion.criterion = 'adhb.L8.admissions', n = eligibility.dt[,length(unique(adhb.theatre.event.id))]),
    # data.table(inclusion.criterion = 'matched.adhb.moh.event', n = eligibility.dt[!is.na(adhb.event.id) & !is.na(moh.event.id),length(unique(moh.event.id))]),
    assess.inclusion.criteria(eligibility.factor.vector),
    data.table(inclusion.criterion = 'time.series.unique', n = eligibility.dt[time.series.eligible.and.unique == TRUE, .N]),
    data.table(inclusion.criterion = 'in.pre.post.period', n = eligibility.dt[(in.pre.period == TRUE |
                                                                                in.post.period == TRUE) & eligible == TRUE, .N]),
    data.table(inclusion.criterion = 'both.pre.post.eligible.first', n = eligibility.dt[both.pre.post.eligible.first == TRUE, .N]),
    data.table(inclusion.criterion = 'in.pre.post.period.unique', n = eligibility.dt[pre.eligible.and.unique | post.eligible.and.unique == TRUE, .N]),
    data.table(inclusion.criterion = 'in.pre.post.period.unique', n = eligibility.dt[pre.eligible.and.unique | post.eligible.and.unique == TRUE, .N]),
    data.table(inclusion.criterion = 'in.pre.period.unique', n = eligibility.dt[pre.eligible.and.unique == TRUE, .N]),
    data.table(inclusion.criterion = 'in.post.period.unique', n = eligibility.dt[post.eligible.and.unique == TRUE, .N])
  )
  
  eligibility.figure.numbers.dt[,diff:=n-shift(n,1,type="lag")]
  
  return(eligibility.figure.numbers.dt)

}
