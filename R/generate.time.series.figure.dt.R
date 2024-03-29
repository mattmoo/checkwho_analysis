##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param risk.adjusted.regression.dt
generate.time.series.figure.dt <- function(risk.adjusted.regression.dt,
                                           ssc.implementation.start,
                                           ssc.implementation.end) {
  
  time.series.figure.dt = risk.adjusted.regression.dt[time.series.eligible.and.unique == TRUE,
                                                   .(
                                                     index.event.id,
                                                     PRIM_HCU,
                                                     # adhb.theatre.event.id,
                                                     adhb.event.id,
                                                     moh.event.id,
                                                     daoh.period.start,
                                                     age,
                                                     age.group,
                                                     ethnicity,
                                                     maori.ethnicity,
                                                     gender,
                                                     asa.acuity,
                                                     acuity,
                                                     clinical.severity,
                                                     CCI,
                                                     asa.status,
                                                     icd.chapter.grouped,
                                                     daoh,
                                                     daoh.risk.adj,
                                                     daoh.emp.logit,
                                                     LOS,
                                                     mort.30.day,
                                                     mort.90.day,
                                                     ethnicity.dra.weight,
                                                     ethnicity.riskgp,
                                                     SSC.dra.weight,
                                                     SSC.riskgp
                                                   )]
  
  time.series.figure.dt[, SSC := 'Implementation']
  
  time.series.figure.dt[daoh.period.start < ssc.implementation.start, SSC := 'Pre']
  time.series.figure.dt[daoh.period.start > ssc.implementation.end, SSC := 'Post']
  
  time.series.figure.dt[, SSC := factor(SSC, levels = c('Pre','Implementation','Post'))]
  
  # Calculate whether cases exceed overall quantiles.
  time.series.figure.dt[,exceeds.daoh.10 := daoh >= quantile(daoh, probs = .1)]
  time.series.figure.dt[,exceeds.daoh.25 := daoh >= quantile(daoh, probs = .25)]
  time.series.figure.dt[,exceeds.daoh.median := daoh >= quantile(daoh, probs = .5)]
  time.series.figure.dt[,exceeds.daoh.75 := daoh >= quantile(daoh, probs = .75)]
  # daoh.dt[,exceeds.daoh.90 := daoh >= quantile(daoh, probs = .9)]
  
  time.series.figure.dt[,exceeds.daoh.risk.adj.10 := daoh.risk.adj >= quantile(daoh.risk.adj, probs = .1)]
  time.series.figure.dt[,exceeds.daoh.risk.adj.25 := daoh.risk.adj >= quantile(daoh.risk.adj, probs = .25)]
  time.series.figure.dt[,exceeds.daoh.risk.adj.median := daoh.risk.adj >= quantile(daoh.risk.adj, probs = .5)]
  time.series.figure.dt[,exceeds.daoh.risk.adj.75 := daoh.risk.adj >= quantile(daoh.risk.adj, probs = .75)]
  
  time.series.figure.dt[,daoh.rank := frank(daoh, ties.method = "average")]
  time.series.figure.dt[,daoh.risk.adj.rank := frank(daoh.risk.adj, ties.method = "average")]
  
  return(time.series.figure.dt)

}
