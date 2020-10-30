##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param eligibility.dt
generate.index.event.dt <- function(eligibility.dt) {

  index.event.dt =
    eligibility.dt[, .(
      index.event.id = 1:.N,
      PRIM_HCU,
      moh.event.id,
      adhb.event.id,
      # adhb.theatre.event.id,
      # adhb.op.date.time,
      moh.op.date,
      time.series.eligible.and.unique,
      pre.eligible.and.unique,
      post.eligible.and.unique
    )][time.series.eligible.and.unique | pre.eligible.and.unique | post.eligible.and.unique]
  
  return(index.event.dt)
}
