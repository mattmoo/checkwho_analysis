##' Transforms a table to flextable, which is probably the easiest format to
##' work with currently.
##'
##' @title
##' @param name Character name by which it will be referenced in
##'   reports.
##' @param input.table A table to transform, currently supports gtsummary.
##' @param caption The table caption.
generate.publication.table <- function(name,
                                       input.table,
                                       caption) {

  supported.formats = c('gtsummary', 'huxtable')
  
  if (!is(input.table, supported.formats)) {
    message(paste0('Input format: ',
                   paste0(class(input.table), collapse = ', ')))
    message(paste0('Supported formats: ',
                   paste0(supported.formats, collapse = ', ')))
    stop(paste('Transformation of input.table to flextable not supported.'))
  }
  
  if (is(input.table, 'gtsummary')) {
    output.ft = gtsummary::as_flex_table(input.table)
  } else if (is(input.table, 'huxtable')) {
    colnames(input.table) = 1:ncol(input.table)
    output.ft = huxtable::as_flextable(input.table)
  }
  
  output.ft = flextable::autofit(output.ft)

  output = list(name = name,
                type = 'table',
                caption = caption,
                ft = output.ft)
  
  return(output)
}
