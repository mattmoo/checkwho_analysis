##' Generates a little table summarising the types of facility.
##'
##' @title
##' @param moh.event.dt
##' @param facility.lookup.dt
draw.facility.type.summary.ht <- function(moh.event.dt, facility.lookup.dt) {
  
  
  facility.type.summary.dt = merge(moh.event.dt, 
                                   facility.lookup.dt, 
                                   by.x = 'FACILITY', 
                                   by.y = 'Health Facility Code',
                                   all.x = TRUE)[, .N, by = `Facility Type Name`][order(-N)]
  
  facility.type.summary.dt = rbind(facility.type.summary.dt[!is.na(`Facility Type Name`)],
                                   facility.type.summary.dt[is.na(`Facility Type Name`)])
  
  facility.type.summary.dt[is.na(`Facility Type Name`), `Facility Type Name` := 'Not specified']
  
  facility.type.summary.dt = rbind(facility.type.summary.dt[,.(`Facility Type Name` = 'Total', N = sum(N))],
                                   facility.type.summary.dt)
  
  facility.type.summary.ht = hux(facility.type.summary.dt, add_colnames = FALSE) %>%
    add_colnames() %>%
    set_bottom_border(row = 1, col = everywhere, value = 1) %>%
    set_top_border(row = 1, col = everywhere, value = 1) %>%
    set_bottom_border(row = nrow(facility.type.summary.dt)+1, col = everywhere, value = 1) %>%
    set_align(row = everywhere, col = 2, value = 1) %>%
    set_number_format(row = everywhere, col = 2, value = fmt_pretty())
  
  return(facility.type.summary.ht)
}
