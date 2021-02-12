##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param pre.post.daoh.statistics.list
draw.pre.post.daoh.statistics.ht <- function(pre.post.daoh.statistics.list) {

  col.headings = c('Group', 'Mean', '(SE)', '0.1', '0.25', '0.5', '0.75', '0.9', 'p')
  
  quantiles.to.get = c(0.1, 0.25, 0.5, 0.75, 0.9)
  
  
  col.headings = c('Group',
                   'Mean',
                   'Mean',
                   '0.1',
                   '0.1',
                   '0.25',
                   '0.25',
                   '0.5',
                   '0.5',
                   '0.75', 
                   '0.75', 
                   '0.9', 
                   '0.9', 
                   'p')
  
  
  quantile.list.to.summary.dt = function(group.quantiles.stats,
                                         group.mean.stats,
                                         quantiles.to.get = c(0.1, 0.25, 0.5, 0.75, 0.9)) {
    
    ci.coef = qnorm(0.975)
    
    quantile.summary.dt = melt(as.data.table(group.quantiles.stats), measure = patterns('^0.', '^se'), value.name = c('value', 'se'))
    quantile.summary.dt[, ci.low := value - ci.coef*se]
    quantile.summary.dt[, ci.high := value + ci.coef*se]
    
    quantile.summary.dt[, quantile := quantiles.to.get[variable]]
    
    setnames(quantile.summary.dt, old = c('quantile','value'), new = c('measure','daoh'))
    
    mean.summary.dt = as.data.table(group.mean.stats)
    mean.summary.dt[, measure := 'mean']
    mean.summary.dt[, ci.low := daoh - ci.coef*se]
    mean.summary.dt[, ci.high := daoh + ci.coef*se]
    
    summary.dt = rbindlist(list(
      quantile.summary.dt,
      mean.summary.dt
    ),
    fill = TRUE,
    use.names = TRUE)
    summary.dt[, sesq := se^2]
    
    diff.dt = summary.dt[, .SD[, .(
      SSC = 'Difference',
      daoh = diff(daoh),
      ci.low = diff(daoh) - ci.coef * sqrt(sum(sesq)),
      ci.high = diff(daoh) + ci.coef * sqrt(sum(sesq))
    )], by = measure]
    
    summary.dt = rbindlist(list(
      summary.dt,
      diff.dt
    ),
    fill = TRUE,
    use.names = TRUE)[order(measure)]
    
    summary.dt[, variable := NULL]
    summary.dt[, ci.char := sprintf("[ %4.5f - %4.5f ]", ci.low, ci.high)]
    
    setcolorder(summary.dt, neworder = c("SSC", "measure", "daoh", "ci.low", "ci.high", "se", "sesq"))
    summary.dt[SSC == 'Pre', SSC := 'Pre-SSC']
    summary.dt[SSC == 'Post', SSC := 'Post-SSC']
    
    summary.dt[, SSC := factor(SSC, levels = c('Pre-SSC', 'Post-SSC', 'Difference'))]
    summary.dt[, measure := factor(measure, levels = c('mean', as.character(quantiles.to.get)))]
    
    
    return(summary.dt)
  }
  
  group.stat.list.to.ht.segment = function(group.stat.list,
                                           segment.title = '') {
    summary.dt = quantile.list.to.summary.dt(group.stat.list$quantiles$group,
                                             group.stat.list$mean$group)
    
    
    summary.dt.long = melt(summary.dt[, .(SSC, measure, daoh = as.character(daoh), ci.char)], id.vars = c('measure', 'SSC'))
    
    summary.dt.tab = dcast(summary.dt.long, SSC ~ measure + variable)
    summary.dt.tab[SSC == 'Difference', p := group.stat.list$stats$p.value]
    
    summary.ht.segment = huxtable(summary.dt.tab, add_colnames = FALSE) %>%
      set_number_format(value = list(function(x)
        ifelse (
          x %% 1 == 0, yes = as.character(x), no = sprintf('%4.2f', x)
        ))) %>%
      set_bold(col = c(1, 14), value = TRUE) %>%
      set_align(col = seq(2, 12, 2), value = 'right') %>%
      set_number_format(col = 14,
                        value = list(function(x)
                          scales::pvalue(x, accuracy = 0.00001))) %>%
      set_bottom_border(row = c(3), value = 1)
    
    if (segment.title != '') {
      segment.title.vec = rep(NA, ncol(summary.ht.segment))
      segment.title.vec[1] = segment.title
      segment.title.ht = t(hux(segment.title.vec)) %>%
        merge_cells(col = 1:ncol(summary.ht.segment)) %>%
        set_bold(value = TRUE)
      
      summary.ht.segment = rbind(segment.title.ht, summary.ht.segment)
    }
    
    summary.ht.segment = summary.ht.segment %>%
      set_top_border(row = 1, value = 1)
    
    return(summary.ht.segment)
    
  }
  
  major.heading = rep(NA, 14)
  major.heading[4] = 'Quantile'
  
  col.headings.1.ht = t(hux(major.heading)) %>%
    set_bold(row = 1, value = TRUE) %>%
    merge_cells(row = 1, col = 4:13) %>%
    set_align(row = 1, value = 'centre') %>%
    set_top_border(row = 1, value = 1)
  
  col.headings.2.ht = t(hux(col.headings)) %>%
    set_bottom_border(row = c(1), value = 1) %>%
    set_bold(value = TRUE) %>%
    set_align(value = 'centre')
  
  for (col.ind in seq(2,12,2)) {
    col.headings.2.ht = merge_cells(col.headings.2.ht, col = c(col.ind, col.ind+1))
  }
  
  pre.post.daoh.statistics.ht = rbind(col.headings.1.ht,
                     col.headings.2.ht,
                     group.stat.list.to.ht.segment(pre.post.daoh.statistics.list$raw, segment.title = 'Unadjusted'),
                     group.stat.list.to.ht.segment(pre.post.daoh.statistics.list$risk.adj, segment.title = 'Risk-adjusted'))
  
  
  return(pre.post.daoh.statistics.ht)
}
