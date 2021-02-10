##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param pre.post.daoh.statistics.list
draw.pre.post.daoh.statistics.ht <- function(pre.post.daoh.statistics.list) {

  col.headings = c('Group', 'Mean', '(SE)', '0.1', '0.25', '0.5', '0.75', '0.9', 'p')
  
  raw.stat.dt = data.table(
    group = pre.post.daoh.statistics.list$raw$mean$group[, SSC],
    mean = pre.post.daoh.statistics.list$raw$mean$group[, mean],
    se = pre.post.daoh.statistics.list$raw$mean$group[, se],
    
    q01 = pre.post.daoh.statistics.list$raw$quantiles$group[quantile == 0.1, value, by = SSC][,value],
    q25 = pre.post.daoh.statistics.list$raw$quantiles$group[quantile == 0.25, value, by = SSC][,value],
    q50 = pre.post.daoh.statistics.list$raw$quantiles$group[quantile == 0.5, value, by = SSC][,value],
    q75 = pre.post.daoh.statistics.list$raw$quantiles$group[quantile == 0.75, value, by = SSC][,value],
    q90 = pre.post.daoh.statistics.list$raw$quantiles$group[quantile == 0.9, value, by = SSC][,value],
    p = NA_real_
    
    # p = pvalue(pre.post.daoh.statistics.list$raw$stats)
  )
  
  col.order = names(raw.stat.dt)
  
  raw.stat.dt = rbindlist(list(
    data.table(group = 'Unadjusted', p = as.numeric(pvalue(pre.post.daoh.statistics.list$raw$stats))),
    raw.stat.dt
  ),
  fill = TRUE,
  use.names = TRUE)
  
  risk.adj.stat.dt = data.table(
    group = data.table(pre.post.daoh.statistics.list$risk.adj$mean$group)[, SSC],
    mean = data.table(pre.post.daoh.statistics.list$risk.adj$mean$group)[, daoh],
    se = data.table(pre.post.daoh.statistics.list$risk.adj$mean$group)[, se],
    
    q01 = pre.post.daoh.statistics.list$risk.adj$quantiles$group[,'0.1'],
    q25 = pre.post.daoh.statistics.list$risk.adj$quantiles$group[,'0.25'],
    q50 = pre.post.daoh.statistics.list$risk.adj$quantiles$group[,'0.5'],
    q75 = pre.post.daoh.statistics.list$risk.adj$quantiles$group[,'0.75'],
    q90 = pre.post.daoh.statistics.list$risk.adj$quantiles$group[,'0.9'],
    p = NA_real_
    
    # p = pvalue(pre.post.daoh.statistics.list$raw$stats)
  )
  
  risk.adj.stat.dt = rbindlist(list(
    data.table(group = 'Risk-adjusted', p = pre.post.daoh.statistics.list$risk.adj$stats$p.value),
    risk.adj.stat.dt
  ),
  fill = TRUE,
  use.names = TRUE)
  
  
  pre.post.daoh.statistics.dt = rbindlist(list(
    raw.stat.dt,
    risk.adj.stat.dt
  ))
  
  setcolorder(pre.post.daoh.statistics.dt, col.order)
  
  pre.post.daoh.statistics.ht = huxtable(pre.post.daoh.statistics.dt, add_colnames = FALSE)
  
  pre.post.daoh.statistics.ht = pre.post.daoh.statistics.ht %>%
    set_number_format(value = fmt_pretty(digits = 4)) %>%
    set_number_format(col = 3, value = "(%.3f)") %>%
    set_bold(row = c(1,4), value = TRUE) %>%
    set_align(row = 1, col = c(2,3,5,6), value = 'right')
  
  
  number_format(pre.post.daoh.statistics.ht)[,9] = list(function(x) scales::pvalue(x, accuracy = 0.00001))
  
  pre.post.daoh.statistics.ht = rbind(
    t(huxtable(col.headings)),
    pre.post.daoh.statistics.ht
  ) %>%
    set_bold(row = c(1), value = TRUE) %>%
    set_align(row = 1, col = 2:9, value = 'right') %>%
    set_align(col = 3, value = 'left')
  
  major.heading = rep(NA, 9)
  major.heading[4] = 'Quantile'
  
  pre.post.daoh.statistics.ht = rbind(
    t(hux(major.heading)),
    pre.post.daoh.statistics.ht
  ) %>%
    set_bold(row = c(1), value = TRUE) %>%
    merge_cells(row = 1, col = 4:8) %>%
    set_align(row = 1, value = 'centre')
  
  pre.post.daoh.statistics.ht = pre.post.daoh.statistics.ht %>%
    set_top_border(row = 1, value = 1) %>%
    set_bottom_border(row = c(2,5,8), value = 1)
  
  pre.post.daoh.statistics.ht[which(pre.post.daoh.statistics.ht[1] == 'Pre'), 1] = 'Pre-SSC'
  pre.post.daoh.statistics.ht[which(pre.post.daoh.statistics.ht[1] == 'Post'), 1] = 'Post-SSC'
  
  
  return(pre.post.daoh.statistics.ht)
}
