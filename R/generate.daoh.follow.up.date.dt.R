##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param date.status.dt
##' @param period.rect.plot
##' @param x.time.scale
##' @param daoh.limits
##' @param round.period
generate.daoh.follow.up.date.dt <-
  function(date.status.dt,
           period.rect.plot,
           x.time.scale,
           daoh.limits,
           cull.days,
           round.period = "1.day") {
    
    follow.up.binomial.dt = date.status.dt[, .(aoh.n = sum(daoh),
                                               dih.n = .N - sum(daoh),
                                               aoh.prop = (sum(daoh))/.N,
                                               dih.prop = (.N - sum(daoh))/.N,
                                               N = .N),
                                           by = .(date = floor_date(date, 'week'))]
    
    follow.up.binomial.dt[, date.numeric := as.numeric(date)]
    
    follow.up.binomial.dt = follow.up.binomial.dt[order(date)][cull.days:(.N - cull.days)]
    
  }
