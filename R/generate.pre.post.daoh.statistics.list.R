##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param pre.post.figure.dt
##' @param pre.post.svy.des
generate.pre.post.daoh.statistics.list <- function(pre.post.figure.dt, pre.post.svy.des) {

  raw.comparison = coin::wilcox_test(daoh~SSC, pre.post.figure.dt, exact = TRUE, detailed = TRUE)
  riskadj.comparison = survey::svyranktest(daoh ~ SSC, pre.post.svy.des, test = "wilcoxon")
  
  quantiles.to.get = c(0.1, 0.25, 0.5, 0.75, 0.9)
  
  raw.quantiles = quantile(pre.post.figure.dt[, daoh], probs = quantiles.to.get)
  
  riskadj.quantiles = svyquantile(
    x = ~ daoh,
    design = pre.post.svy.des,
    quantiles = quantiles.to.get,
    ci = TRUE
  )
  
  raw.group.quantiles = pre.post.figure.dt[, .(quantile = quantiles.to.get,
                                               value = quantile(daoh, probs = quantiles.to.get)), by = SSC]
  
  riskadj.group.quantiles = survey::svyby(
    formula = ~ daoh,
    by = ~ SSC,
    design = pre.post.svy.des,
    survey::svyquantile,
    quantiles = quantiles.to.get,
    ties = 'discrete',
    ci = TRUE
  )
  
  
  raw.mean = pre.post.figure.dt[, .(mean = mean(daoh),
                                    se = std.err(daoh))]
  
  raw.group.means = pre.post.figure.dt[, .(mean = mean(daoh),
                                           se = std.err(daoh)), by = SSC]
  
  riskadj.mean = survey::svymean(x  = ~daoh, design = pre.post.svy.des)
  
  riskadj.group.means = survey::svyby(
    formula = ~ daoh,
    by = ~ SSC,
    design = pre.post.svy.des,
    survey::svymean,
    ci = TRUE
  )
  
  pre.post.daoh.statistics.list = list(
    raw = list(
      quantiles = list(
        overall = raw.quantiles,
        group = raw.group.quantiles
      ),
      mean = list(
        overall = raw.mean,
        group = raw.group.means
      ),
      stats = raw.comparison
      
    ),
    risk.adj = list(
      quantiles = list(
        overall = riskadj.quantiles,
        group = riskadj.group.quantiles
      ),
      mean = list(
        overall = riskadj.mean,
        group = riskadj.group.means
      ),
      stats = riskadj.comparison
      
    )
  )
  
  return(pre.post.daoh.statistics.list)
  
}
