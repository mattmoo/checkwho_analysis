##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param daoh.dt
generate.monthly.summary.dt <- function(daoh.dt) {

  setDT(daoh.dt)
  daoh.dt[,daoh.start := floor_date(daoh.period.start, unit = 'months')]
  daoh.dt[,exceeds.daoh.10 := daoh >= quantile(daoh, probs = .1)]
  daoh.dt[,exceeds.daoh.25 := daoh >= quantile(daoh, probs = .25)]
  daoh.dt[,exceeds.daoh.median := daoh >= quantile(daoh, probs = .5)]
  daoh.dt[,exceeds.daoh.75 := daoh >= quantile(daoh, probs = .75)]
  daoh.dt[,exceeds.daoh.90 := daoh >= quantile(daoh, probs = .9)]
  
  monthly.summary.dt = daoh.dt[, .(n = .N,
                                   daoh.10 = quantile(daoh, probs = .1),
                                   daoh.25 = quantile(daoh, probs = .25),
                                   daoh.median = median(daoh),
                                   daoh.75 = quantile(daoh, probs = .75),
                                   daoh.90 = quantile(daoh, probs = .9),
                                   mort.30.day = mean(mort.30.day),
                                   mort.90.day = mean(mort.90.day),
                                   exceeds.daoh.10 = mean(exceeds.daoh.10),
                                   exceeds.daoh.25 = mean(exceeds.daoh.25),
                                   exceeds.daoh.median = mean(exceeds.daoh.median),
                                   exceeds.daoh.75 = mean(exceeds.daoh.75),
                                   exceeds.daoh.90 = mean(exceeds.daoh.90)),
                               by = daoh.start]

  # monthly.summary.dt[, .(
  #   exceeds
  # )]
  
  
  return(monthly.summary.dt[order(daoh.start)])
}
