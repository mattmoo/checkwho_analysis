##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param hypothesis.testing.list
##' @param changepoint.measure.list
draw.bayes.hypothesis.testing.ht <- function(hypothesis.testing.list,
                                             changepoint.measure.list,
                                             changepoint.model.list) {
  
  month.year = function(date.numeric) {
    result = paste0(
      lubridate::month(
        as.Date(date.numeric),
        label = TRUE,
        abbr = TRUE
      ),
      ' ',
      lubridate::year(as.Date(date.numeric))
    )
    return(result)
  }
  
  # hyp.ind = 1
  generate.hyp.row = function(hyp.ind) {
    hyp.test.name = names(hypothesis.testing.list)[hyp.ind]
    hyp.test = hypothesis.testing.list[[hyp.ind]]
    hyp.pretty.test.name = names(changepoint.measure.list)[names(hypothesis.testing.list) == hyp.test.name]
    
    
    
    zero.hyp.test = hypothesis(changepoint.model.list[[hyp.test.name]],
                               "cp_1 = 0")
    
    hyp.row.dt = data.table(
      Measure = hyp.pretty.test.name,
      `Estimated changepoint` = paste0(
        month.year(zero.hyp.test$mean)
      ), 
      `[95% credible interval]` = paste0(
        ' [',
        month.year(zero.hyp.test$lower),
        '-',
        month.year(zero.hyp.test$upper),
        ']'
      ),
      `During Implementation` = 
        as.data.table(hyp.test)[stringr::str_detect(string = hyp.test$hypothesis,
                                                    pattern = '.+>= 13818.+<'),
                                BF],
      `During Post-SSC` = 
        as.data.table(hyp.test)[stringr::str_detect(string = hyp.test$hypothesis,
                                                    pattern = '.+>= 14365.+<'),
                                BF],
      `Outside Implementation and Post-SSC` =
        as.data.table(hyp.test)[stringr::str_detect(string = hyp.test$hypothesis,
                                                    pattern = '.+<.+>'),
                                BF]
      # `During SSC implementation` = as.data.table(hyp.test)[stringr::str_detect(string = hyp.test$hypothesis,
      #                                                                           pattern = '.+<.+>',
      #                                                                           negate = TRUE),
      #                                                       BF],
      # `Outside SSC implementation` = as.data.table(hyp.test)[stringr::str_detect(string = hyp.test$hypothesis, pattern = '.+<.+>'),
      #                                                        BF]
    )
    
  }
  
  hyp.testing.dt = rbindlist(lapply(1:length(hypothesis.testing.list), FUN = generate.hyp.row))
  
  # hyp.testing.dt[, Ratio := `Changepoint model weight` / `Null model weight`]
  
  hyp.testing.ht = huxtable::huxtable(hyp.testing.dt) %>%
    huxtable::set_number_format(everywhere, 4:6, 3) %>%
    huxtable::set_top_border(1, everywhere, 1) %>%
    huxtable::set_bottom_border(1, everywhere, 1) %>%
    huxtable::set_bottom_border(nrow(hyp.testing.dt)+1, everywhere, 1) %>%
    huxtable::set_bold(1, everywhere, TRUE)
  
  hyp.testing.ht = rbind(huxtable(a = NA,
                                  a2 = NA,
                                  a3 = NA,
                                  b = 'Changepoint detected (Bayes factor)',
                                  c = NA,
                                  d = NA),
                         hyp.testing.ht) %>%
    huxtable::merge_cells(2, 4:6) %>%
    # huxtable::merge_cells(2:3, 2) %>%
    huxtable:: set_align(2, everywhere, 'centre') %>%
    huxtable::set_top_border(2, everywhere, 1) %>%
    huxtable::set_bold(2, everywhere, TRUE)
  
  hyp.testing.ht = hyp.testing.ht[2:nrow(hyp.testing.ht),]
}
