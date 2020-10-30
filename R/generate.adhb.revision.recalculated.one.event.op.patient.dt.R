##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param adhb.event.op.patient.dt
generate.adhb.revision.one.recalculated.event.op.patient.dt <-
  function(adhb.event.op.patient.dt,
           pre.period.start,
           pre.period.end,
           post.period.start,
           post.period.end,
           adhb.revision.one.event.op.patient.dt) {
    
    
    adhb.revision.one.recalculated.event.op.patient.dt = copy(adhb.event.op.patient.dt)
    
    adhb.revision.one.recalculated.event.op.patient.dt[,`Actual Into Theatre Date Time` := as_datetime(`Actual Into Theatre Date Time`)]
    
    adhb.revision.one.recalculated.event.op.patient.dt[,in.pre.period := (`Actual Into Theatre Date Time` >= pre.period.start &
                                                                            `Actual Into Theatre Date Time` <= pre.period.end)]
    adhb.revision.one.recalculated.event.op.patient.dt[,in.post.period := (`Actual Into Theatre Date Time` >= post.period.start &
                                                                             `Actual Into Theatre Date Time` <= post.period.end)]
    adhb.revision.one.recalculated.event.op.patient.dt = adhb.revision.one.recalculated.event.op.patient.dt[in.pre.period |
                                                                                                              in.post.period][order(`Actual Into Theatre Date Time`)]
    
    # adhb.revision.one.recalculated.event.op.patient.dt[,age.years := as.numeric(lubridate::interval(`Date of Birth`,`Actual Into Theatre Date Time`,)/years(1))]
    # adhb.revision.one.recalculated.event.op.patient.dt = adhb.revision.one.recalculated.event.op.patient.dt[age.years >= 16]
    
    
    adhb.revision.one.recalculated.event.op.patient.dt[, in.pre.period.first.eligible := FALSE]
    adhb.revision.one.recalculated.event.op.patient.dt[, in.post.period.first.eligible := FALSE]
    adhb.revision.one.recalculated.event.op.patient.dt = rbindlist(list(
      adhb.revision.one.recalculated.event.op.patient.dt[in.pre.period == TRUE, .SD[`Actual Into Theatre Date Time` == min(`Actual Into Theatre Date Time`)], by = NHI],
      adhb.revision.one.recalculated.event.op.patient.dt[in.post.period == TRUE, .SD[`Actual Into Theatre Date Time` == min(`Actual Into Theatre Date Time`)], by = NHI]
      
    ))
    
    
    missing.from.new = adhb.revision.one.event.op.patient.dt[!EventNumber %in% adhb.revision.one.recalculated.event.op.patient.dt[,`Event ID`]]
    missing.from.old = adhb.revision.one.recalculated.event.op.patient.dt[!`Event ID` %in% adhb.revision.one.event.op.patient.dt[,EventNumber]]
    print(missing.from.new[,.N])
    print(missing.from.old[,.N])
    
    return(adhb.revision.one.recalculated.event.op.patient.dt)
  }
