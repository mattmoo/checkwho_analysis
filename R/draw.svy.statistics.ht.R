##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param pre.post.daoh.statistics.list
##' @param by.group
##' @param calculate.difference
##' @param suppress.quantile.cis
draw.svy.statistics.ht <-
  function(svy.statistics.list,
           by.group,
           calculate.difference = TRUE,
           suppress.quantile.cis = TRUE) {
    
    col.headings = c('Group', 'Mean', '(SE)', '0.1', '0.25', '0.5', '0.75', '0.9', 'p')
    
    quantiles.to.get = c(0.1, 0.25, 0.5, 0.75, 0.9)
    
    
    col.headings = c(
      'Group',
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
      'p'
    )
    
    
    quantile.list.to.summary.dt = function(group.quantiles.stats,
                                           group.mean.stats,
                                           quantiles.to.get = c(0.1, 0.25, 0.5, 0.75, 0.9),
                                           calculate.difference = TRUE) {
      
      
      if (nrow(group.quantiles.stats) != 2) {
        if (calculate.difference) {
          warning("There are not two levels being summarised, difference will not be calculated.")
          calculate.difference = FALSE
        }
      }
      
      
      ci.coef = qnorm(0.975)
      
      quantile.summary.dt = melt(
        as.data.table(group.quantiles.stats),
        measure = patterns('^0.', '^se'),
        value.name = c('value', 'se')
      )
      quantile.summary.dt[, ci.low := value - ci.coef * se]
      quantile.summary.dt[, ci.high := value + ci.coef * se]
      
      by.group = names(quantile.summary.dt)[1]
      
      quantile.summary.dt[, quantile := quantiles.to.get[variable]]
      
      setnames(
        quantile.summary.dt,
        old = c('quantile', 'value'),
        new = c('measure', 'daoh')
      )
      
      mean.summary.dt = as.data.table(group.mean.stats)
      mean.summary.dt[, measure := 'mean']
      mean.summary.dt[, ci.low := daoh - ci.coef * se]
      mean.summary.dt[, ci.high := daoh + ci.coef * se]
      
      summary.dt = rbindlist(
        list(quantile.summary.dt,
             mean.summary.dt),
        fill = TRUE,
        use.names = TRUE
      )
      summary.dt[, sesq := se ^ 2]
      
      # Give the column of variable a common name.
      setnames(
        summary.dt,
        old = c(by.group),
        new = c('group.name')
      )
      
      
      if (calculate.difference) {
        diff.dt = summary.dt[, .SD[, .(
          group.name = 'Difference',
          daoh = diff(daoh),
          ci.low = diff(daoh) - ci.coef * sqrt(sum(sesq)),
          ci.high = diff(daoh) + ci.coef * sqrt(sum(sesq))
        )], by = measure]
      } else {
        diff.dt = NULL
      }
      
      summary.dt = rbindlist(list(summary.dt,
                                  diff.dt),
                             fill = TRUE,
                             use.names = TRUE)[order(measure)]
      
      summary.dt[, variable := NULL]
      summary.dt[, ci.char := sprintf("[%4.5f , %4.5f]", ci.low, ci.high)]
      
      setcolorder(summary.dt,
                  neworder = c("group.name", "measure", "daoh", "ci.low", "ci.high", "se", "sesq"))
      summary.dt[, measure := factor(measure, levels = c('mean', as.character(quantiles.to.get)))]
      
      
      # Change the common name back, could be a bit more gracefully implemented.
      setnames(
        summary.dt,
        old = c('group.name'),
        new = c(by.group)
      )
      
      return(summary.dt)
    }
    
    group.stat.list.to.ht.segment = function(group.stat.list,
                                             by.group,
                                             segment.title = '',
                                             calculate.difference = TRUE,
                                             suppress.quantile.cis = TRUE) {
      
      
      summary.dt = quantile.list.to.summary.dt(group.stat.list$quantiles$group,
                                               group.stat.list$mean$group,
                                               calculate.difference = calculate.difference)
      
      
      summary.dt.long = melt(summary.dt[, .(by.group = get(by.group),
                                            measure,
                                            daoh = as.character(daoh),
                                            ci.char)],
                             id.vars = c('measure', 'by.group'))
      
      # Eliminate CIs for quantiles if requested.
      if (suppress.quantile.cis) {
        summary.dt.long[measure != 'mean' & variable != 'daoh' & by.group != 'Difference', value := '']
      }
      
      setnames(
        summary.dt.long,
        old = c('by.group'),
        new = c(by.group)
      )
      
      summary.dt.tab = dcast(summary.dt.long, as.formula(paste0(by.group, ('~ measure + variable'))))
      summary.dt.tab[get(by.group) == 'Difference', p := group.stat.list$stats$p.value]
      
      daoh.col.inds = which(names(summary.dt.tab) %like% 'daoh')
      ci.col.inds = which(names(summary.dt.tab) %like% 'ci.char')
      p.col.inds = which(names(summary.dt.tab) == 'p')
      
      summary.ht.segment = huxtable(summary.dt.tab, add_colnames = FALSE) %>%
        set_number_format(value = list(function(x)
          ifelse (
            x %% 1 == 0, yes = as.character(x), no = sprintf('%4.2f', x)
          ))) %>%
        set_bold(col = c(1, 14), value = TRUE) %>%
        set_align(col = seq(2, 12, 2), value = 'right') %>%
        set_number_format(col = 14,
                          value = list(function(x)
                            scales::pvalue(x, accuracy = 0.0001))) %>%
        set_bottom_border(row = c(nrow(summary.dt.tab)), value = 1)
      
      # Put in a segment title if provided.
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
    
    for (col.ind in seq(2, 12, 2)) {
      col.headings.2.ht = merge_cells(col.headings.2.ht, col = c(col.ind, col.ind +
                                                                   1))
    }
    
    svy.statistics.ht = rbind(
      col.headings.1.ht,
      col.headings.2.ht,
      group.stat.list.to.ht.segment(svy.statistics.list$raw,
                                    by.group = by.group,
                                    segment.title = 'Unadjusted',
                                    calculate.difference = calculate.difference,
                                    suppress.quantile.cis = suppress.quantile.cis),
      group.stat.list.to.ht.segment(svy.statistics.list$risk.adj,
                                    by.group = by.group,
                                    segment.title = 'Risk-adjusted',
                                    calculate.difference = calculate.difference,
                                    suppress.quantile.cis = suppress.quantile.cis)
    )
    
    # Remove p column if no difference row.
    if (!calculate.difference) {
      svy.statistics.ht[,14] = NULL
    }
    
    
    return(svy.statistics.ht)

}
