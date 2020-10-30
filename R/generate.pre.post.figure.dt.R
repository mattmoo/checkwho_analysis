##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param risk.adjusted.regression.dt
generate.pre.post.figure.dt <- function(risk.adjusted.regression.dt) {
  
  pre.post.figure.dt = risk.adjusted.regression.dt[pre.eligible.and.unique == TRUE |
                                                     post.eligible.and.unique == TRUE,
                                                   .(
                                                     PRIM_HCU,
                                                     # adhb.theatre.event.id,
                                                     adhb.event.id,
                                                     moh.event.id,
                                                     pre.eligible.and.unique,
                                                     post.eligible.and.unique,
                                                     daoh.period.start,
                                                     age.group,
                                                     ethnicity,
                                                     maori.ethnicity,
                                                     gender,
                                                     asa.acuity,
                                                     acuity,
                                                     CCI,
                                                     asa.status,
                                                     icd.chapter.grouped,
                                                     daoh,
                                                     daoh.risk.adj,
                                                     mort.30.day,
                                                     mort.90.day
                                                   )]
  
  pre.post.figure.dt[, SSC := NA_character_]
  
  pre.post.figure.dt[pre.eligible.and.unique == TRUE, SSC := 'Pre']
  pre.post.figure.dt[post.eligible.and.unique == TRUE, SSC := 'Post']
  
  pre.post.figure.dt[, SSC := factor(SSC, levels = c('Pre','Post'))]

  pre.post.figure.dt[, pre.eligible.and.unique := NULL]
  pre.post.figure.dt[, post.eligible.and.unique := NULL]
  
  # Calculate whether cases exceed overall quantiles.
  pre.post.figure.dt[,exceeds.daoh.10 := daoh >= quantile(daoh, probs = .1)]
  pre.post.figure.dt[,exceeds.daoh.25 := daoh >= quantile(daoh, probs = .25)]
  pre.post.figure.dt[,exceeds.daoh.median := daoh >= quantile(daoh, probs = .5)]
  pre.post.figure.dt[,exceeds.daoh.75 := daoh >= quantile(daoh, probs = .75)]
  # daoh.dt[,exceeds.daoh.90 := daoh >= quantile(daoh, probs = .9)]
  
  pre.post.figure.dt[,exceeds.daoh.risk.adj.10 := daoh.risk.adj >= quantile(daoh.risk.adj, probs = .1)]
  pre.post.figure.dt[,exceeds.daoh.risk.adj.25 := daoh.risk.adj >= quantile(daoh.risk.adj, probs = .25)]
  pre.post.figure.dt[,exceeds.daoh.risk.adj.median := daoh.risk.adj >= quantile(daoh.risk.adj, probs = .5)]
  pre.post.figure.dt[,exceeds.daoh.risk.adj.75 := daoh.risk.adj >= quantile(daoh.risk.adj, probs = .75)]
  
  pre.post.figure.dt[,daoh.rank := frank(daoh, ties.method = "average")]
  pre.post.figure.dt[,daoh.risk.adj.rank := frank(daoh.risk.adj, ties.method = "average")]
  
  return(pre.post.figure.dt)

}
