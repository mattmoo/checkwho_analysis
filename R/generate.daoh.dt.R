##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param index.event.dt
##' @param moh.patient.dt
##' @param hospitalisation.dt
##' @param daoh.limits
generate.daoh.dt <- function(index.event.dt, 
                             moh.patient.dt,
                             hospitalisation.dt, 
                             daoh.limits) {

  setDT(index.event.dt)
  
  consolidated.hospitalisation.dt = consolidate.events(index.op.dt = index.event.dt,
                                                       event.dt = hospitalisation.dt,
                                                       patient.dt = moh.patient.dt,
                                                       daoh.limits = daoh.limits,
                                                       patient.id.col.name = 'PRIM_HCU',
                                                       index.event.date.col.name = 'moh.op.date',
                                                       dod.col.name = 'date_of_death')
  
  daoh.dt = calculate.daoh(index.op.dt = index.event.dt,
                           patient.dt = moh.patient.dt,
                           daoh.event.dt = consolidated.hospitalisation.dt,
                           patient.id.col.name = 'PRIM_HCU',
                           dod.col.name = 'date_of_death'
  )
  daoh.dt[,mort.30.day := !is.na(date_of_death) & (as.numeric(interval(daoh.period.start, date_of_death)/days(1)) <= 30)]
  daoh.dt[,mort.90.day := !is.na(date_of_death) & (as.numeric(interval(daoh.period.start, date_of_death)/days(1)) <= 90)]
  # 
  # # Calculate whether cases exceed overall quantiles.
  # daoh.dt[,exceeds.daoh.10 := daoh >= quantile(daoh, probs = .1)]
  # daoh.dt[,exceeds.daoh.25 := daoh >= quantile(daoh, probs = .25)]
  # daoh.dt[,exceeds.daoh.median := daoh >= quantile(daoh, probs = .5)]
  # daoh.dt[,exceeds.daoh.75 := daoh >= quantile(daoh, probs = .75)]
  # # daoh.dt[,exceeds.daoh.90 := daoh >= quantile(daoh, probs = .9)]
  
  return(daoh.dt)

}
