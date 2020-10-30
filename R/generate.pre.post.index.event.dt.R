##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param eligibility.dt
generate.pre.post.index.event.dt <- function(pre.post.eligibility.dt) {

  index.event.dt =
    pre.post.eligibility.dt[, .(
      index.event.id = 1:.N,
      PRIM_HCU,
      moh.event.id,
      adhb.event.id,
      # adhb.theatre.event.id,
      # adhb.op.date.time,
      moh.op.date,
      pre.eligible.and.unique,
      post.eligible.and.unique
    )][pre.eligible.and.unique | post.eligible.and.unique]
  
  return(index.event.dt)

}
