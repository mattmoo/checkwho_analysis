##' Generates a huxtable summarising LOO-CV of mcp models.
##'
##' @title
##' @param loo.testing.list
##' @param changepoint.measure.list

draw.loo.testing.ht = function(loo.testing.list,
                               changepoint.measure.list) {
  
  generate.loo.row = function(loo.ind) {
    loo.test.name = names(loo.testing.list)[loo.ind]
    loo.test = loo.testing.list[[loo.ind]]
    loo.pretty.test.name = names(changepoint.measure.list)[names(loo.testing.list) == loo.test.name]
    
    loo.row.dt = data.table(
      Measure = loo.pretty.test.name,
      `Null model weight` = loo.test$loo.compare[model == 'model1', weight],
      `Changepoint model weight` = loo.test$loo.compare[model == 'model2', weight]
    )
    
  }
  
  loo.testing.dt = rbindlist(lapply(1:length(loo.testing.list), FUN = generate.loo.row))
  
  # loo.testing.dt[, Ratio := `Changepoint model weight` / `Null model weight`]
  
  loo.testing.ht = huxtable::huxtable(loo.testing.dt) %>%
    set_number_format(everywhere, 2:3, 3) %>%
    set_top_border(1, everywhere, 1) %>%
    set_bottom_border(1, everywhere, 1) %>%
    set_bottom_border(nrow(loo.testing.dt)+1, everywhere, 1) %>%
    set_bold(1, everywhere, TRUE)
  
}