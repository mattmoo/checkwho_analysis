##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param pre.post.figure.dt
##' @param pre.post.svy.des
generate.pre.post.daoh.statistics.list <- function(pre.post.raw.svy.des, pre.post.svy.des) {

  get.svy.des.stats = function(svy.des,
                               quantiles.to.get = c(0.1, 0.25, 0.5, 0.75, 0.9)) {
    result = list(
      quantiles = list(
        overall = svyquantile(
          x = ~ daoh,
          design = svy.des,
          quantiles = quantiles.to.get,
          ci = TRUE
        ),
        group = survey::svyby(
          formula = ~ daoh,
          by = ~ SSC,
          design = svy.des,
          survey::svyquantile,
          quantiles = quantiles.to.get,
          ties = 'discrete',
          ci = TRUE
        )
      ),
      mean = list(
        overall = survey::svymean(x  = ~ daoh, design = svy.des),
        group = survey::svyby(
          formula = ~ daoh,
          by = ~ SSC,
          design = svy.des,
          survey::svymean,
          ci = TRUE
        )
      ),
      stats = survey::svyranktest(daoh ~ SSC, svy.des, test = "wilcoxon")
    )
    
  }
  
  
  pre.post.daoh.statistics.list = list(
    raw = get.svy.des.stats(pre.post.raw.svy.des),
    risk.adj = get.svy.des.stats(pre.post.svy.des)
  )
  
  
  return(pre.post.daoh.statistics.list)
  
}
