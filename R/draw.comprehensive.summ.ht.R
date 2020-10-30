##' DAOH has a weird distribution, draw up a table with a few different measures
##' of the distribution.
##'
##' @title
##' @param pre.post.figure.dt Data.table for the pre/post comparison
##' @param time.series.figure.dt Data.table 
##' @param n.iterations
draw.comprehensive.summ.ht = function(pre.post.figure.dt,
                                      time.series.figure.dt,
                                      measure.list = list('DAOH\u2089\u2080 (unadjusted)' = 'daoh',
                                                          'DAOH\u2089\u2080 (risk-adjusted)' = 'daoh.risk.adj'),
                                      n.iterations) {
  summ.func.list = list(
    'Min' = min,
    'Max' = max,
    'Mean' = mean,
    'SD' = sd,
    # 'Mean (SD)' = function(x)
    #   paste0(mean(x), digits = 1, ' (', sd(x), digits = 1, ')'),
    '10%' = function(x)
      as.numeric(quantile(x, probs = 0.1)),
    '25%' = function(x)
      as.numeric(quantile(x, probs = 0.25)),
    'Median' = function(x)
      as.numeric(quantile(x, probs = 0.5)),
    '75%' = function(x)
      as.numeric(quantile(x, probs = 0.75)),
    '90%' = function(x)
      as.numeric(quantile(x, probs = 0.9))
  )
  
  data.dt.list = list(
    'Time-series' = time.series.figure.dt,
    'Pre-SSC' = pre.post.figure.dt[SSC == 'Pre'],
    'Post-SSC' = pre.post.figure.dt[SSC == 'Post']
  )
  
  general.format.func = function(x)
    formatC(x,
            format = 'f',
            digits = 2,
            drop0trailing = TRUE)
  diff.format.func = function(x) {
    sign.str = ''
    if (x > 0)
      sign.str = '+'
    paste0(sign.str, general.format.func(x))
  }
  
  summ.ht = NULL
  
  for (measure.ind in 1:length(measure.list)) {
    measure = measure.list[[measure.ind]]
    
    measure.summ.dt = data.table(measure = c(names(measure.list)[measure.ind], names(summ.func.list)))
    
    for (data.dt.ind in 1:length(data.dt.list)) {
      data.dt = data.dt.list[[data.dt.ind]][, get(measure)]
      
      result.dt = data.table(c(NA, lapply(summ.func.list, function(f)
        f(data.dt))))
      
      setnames(result.dt,
               old = 'V1',
               new = names(data.dt.list)[data.dt.ind])
      # print(result.dt)
      measure.summ.dt = cbind(measure.summ.dt, result.dt)
    }
    
    measure.summ.dt[, Difference := as.numeric(`Post-SSC`) - as.numeric(`Pre-SSC`)]
    
    # stat.list[[1]] = wilcox.test(formula = as.formula(paste(measure,'~', 'SSC')), data = pre.post.figure.dt)
    stat.list = list()
    stat.list[[1]] = perm.test(
      x = pre.post.figure.dt[SSC == 'Pre', get(measure)],
      y = pre.post.figure.dt[SSC == 'Post', get(measure)],
      fun = function(x, y)
        wilcox.test(y, x)$statistic,
      trials = n.iterations
    )
    for (summ.func.ind in 1:length(summ.func.list)) {
      stat.list[[summ.func.ind + 1]] = perm.test(
        x = pre.post.figure.dt[SSC == 'Pre', get(measure)],
        y = pre.post.figure.dt[SSC == 'Post', get(measure)],
        fun = function(x, y)
          summ.func.list[[summ.func.ind]](y) - summ.func.list[[summ.func.ind]](x),
        trials = n.iterations
      )
    }
    
    stat.dt = data.table(p = unlist(lapply(
      X = stat.list,
      FUN = function(x)
        x$p.value
    )))
    
    measure.summ.dt = cbind(measure.summ.dt, stat.dt)
    
    # Make a mean/sd row
    mean.sd.row = measure.summ.dt[measure == 'Mean']
    for (col.ind in 1:4) {
      cell.contents = paste0(unlist(measure.summ.dt[measure %like% 'Mean', col.ind, with = FALSE]),
                             ' (',
                             unlist(measure.summ.dt[measure == 'SD', col.ind, with = FALSE]),
                             ')')
      set(
        x = measure.summ.dt,
        i = which(measure.summ.dt[, measure %like% 'Mean']),
        j = col.ind,
        value = cell.contents
      )
    }
    measure.summ.dt = measure.summ.dt[measure != 'SD']
    
    # summ.dt = rbind(summ.dt, measure.summ.dt)
    
    if (is.null(summ.ht)) {
      measure.summ.ht = hux(measure.summ.dt)
      measure.summ.ht = measure.summ.ht %>%
        set_top_border(row = 1,
                       col = everywhere,
                       value = 1) %>%
        set_bottom_border(row = 1,
                          col = everywhere,
                          value = 1) %>%
        set_bottom_border(
          row = nrow(measure.summ.ht),
          col = everywhere,
          value = 1
        ) %>%
        set_bold(row = 1:2,
                 col = everywhere,
                 value = TRUE) %>%
        merge_cells(row = 2, col = 1:2)
      measure.summ.ht[1, 1] = ''
    } else {
      measure.summ.ht = hux(measure.summ.dt, add_colnames = FALSE)
      measure.summ.ht = measure.summ.ht %>%
        set_top_border(row = 1,
                       col = everywhere,
                       value = 1) %>%
        set_bottom_border(
          row = nrow(measure.summ.ht),
          col = everywhere,
          value = 1
        ) %>%
        set_bold(row = 1,
                 col = everywhere,
                 value = TRUE) %>%
        merge_cells(row = 1, col = 1:2)
    }
    
    measure.summ.ht = measure.summ.ht %>%
      set_number_format(
        row = everywhere,
        col = everywhere,
        value = list(general.format.func)
      ) %>%
      set_number_format(row = everywhere,
                        col = 5,
                        value = list(diff.format.func)) %>%
      set_number_format(
        row = everywhere,
        col = 6,
        value = list(function(x)
          scales::pvalue(x, accuracy = 0.0001))
      ) %>%
      set_align(row = everywhere,
                col = 2:4,
                value = 'right')
    
    
    if (is.null(summ.ht)) {
      summ.ht = measure.summ.ht
    } else {
      summ.ht = rbind(summ.ht, measure.summ.ht)
    }
    
  }
  
  # This is now in the 
  # summ.ht = summ.ht %>%
  #   add_footnote(
  #     text = paste0(
  #       "Differences in the overall DAOH\u2089\u2080 distribution were assessed using Wilcoxon-Mann-Whitney U tests. ",
  #       "Differences in other values were assessed using absolute difference. Significance assessed using permutation tests with ",
  #       # format(n.iterations, big.mark = ','),
  #       n.iterations,
  #       " permutations. "
  #     )
  #   )
  
  summ.ht = summ.ht %>%
    set_number_format(row = nrow(summ.ht),
                      col = everywhere,
                      value = fmt_pretty())
  
  return(summ.ht)
}

