##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'

##' @title
##' @param raw.svy.des Survey design with no weighting
##' @param svy.des Survey design with weighting for risk adjustment.
generate.svy.statistics.list <-
  function(raw.svy.des,
           svy.des,
           by.group = 'SSC',
           outcome = 'daoh') {
    
    get.svy.des.stats = function(svy.des,
                                 quantiles.to.get = c(0.1, 0.25, 0.5, 0.75, 0.9)) {
      result = list(
        quantiles = list(
          overall = svyquantile(
            x = as.formula(paste0('~', outcome)),
            design = svy.des,
            quantiles = quantiles.to.get,
            ci = TRUE
          ),
          group = survey::svyby(
            formula = as.formula(paste0('~', outcome)),
            by = as.formula(paste0('~', by.group)),
            design = svy.des,
            survey::svyquantile,
            quantiles = quantiles.to.get,
            ties = 'discrete',
            ci = TRUE
          )
        ),
        mean = list(
          overall = survey::svymean(x  = as.formula(paste0('~', outcome)), design = svy.des),
          group = survey::svyby(
            formula = as.formula(paste0('~', outcome)),
            by = as.formula(paste0('~', by.group)),
            design = svy.des,
            survey::svymean,
            ci = TRUE
          )
        ),
        stats = survey::svyranktest(as.formula(paste0(
          outcome, '~', by.group Thanks
        )), svy.des, test = "wilcoxon")
      )
      
    }
    
    
    pre.post.daoh.statistics.list = list(raw = get.svy.des.stats(raw.svy.des),
                                         risk.adj = get.svy.des.stats(svy.des))
    
    
    return(pre.post.daoh.statistics.list)
    
  }
