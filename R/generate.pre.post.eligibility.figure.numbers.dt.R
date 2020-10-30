##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param eligibility.dt
##' @param eligibility.factor.vector
generate.pre.post.eligibility.figure.numbers.dt <-
  function(eligibility.dt,
           eligibility.factor.vector) {
    
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
    
    pre.post.eligibility.figure.numbers.dt = rbind(
      data.table(inclusion.criterion = 'first.admission.first.op.in.adhb.L8.admissions', n = eligibility.dt[,length(unique(adhb.event.id))]),
      data.table(inclusion.criterion = 'matched.to.moh.op.plus.minus.one.day', n = eligibility.dt[!is.na(adhb.event.id) & !is.na(moh.event.id),length(unique(moh.event.id))]),
      assess.inclusion.criteria(eligibility.factor.vector),
      # data.table(inclusion.criterion = 'time.series.unique', n = eligibility.dt[time.series.eligible.and.unique == TRUE, .N])
      data.table(inclusion.criterion = 'moh.opdate.in.pre.post.period', n = eligibility.dt[(in.pre.period == TRUE |
                                                                                  in.post.period == TRUE) & eligible == TRUE, .N]),
      data.table(inclusion.criterion = 'in.pre.post.period.unique', n = eligibility.dt[pre.eligible.and.unique | post.eligible.and.unique == TRUE, .N]),
      data.table(inclusion.criterion = 'in.pre.period.unique', n = eligibility.dt[pre.eligible.and.unique == TRUE, .N]),
      data.table(inclusion.criterion = 'in.post.period.unique', n = eligibility.dt[post.eligible.and.unique == TRUE, .N])
    )
    
    pre.post.eligibility.figure.numbers.dt[,diff:=n-shift(n,1,type="lag")]
    
    return(pre.post.eligibility.figure.numbers.dt)
    
  }
