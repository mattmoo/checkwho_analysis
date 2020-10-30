##' Generate demographic table
##'
##' @title
##' @param pre.post.figure.dt
##' @param time.series.figure.dt
draw.demographic.table.html <- function(pre.post.figure.dt, 
                                        time.series.figure.dt) {
  
  pre.post.figure.dt[SSC == 'Pre', demo.group := 'Pre-SSC']
  pre.post.figure.dt[SSC == 'Post', demo.group := 'Post-SSC']
  time.series.figure.dt[, demo.group := 'Time-series']
  time.series.figure.dt[, demo.group := factor(demo.group,
                                               levels = c('Pre-SSC',
                                                          'Post-SSC',
                                                          'Time-series'))]

  demo.dt = rbindlist(
    list(
      pre.post.figure.dt,
      time.series.figure.dt
    )
  )
  
  table1::label(demo.dt$gender) = 'Gender'
  table1::label(demo.dt$age.group) = 'Age group'
  table1::label(demo.dt$ethnicity) = 'Ethnic group'
  
  demographic.table.html = table1::table1(
    ~ gender + age.group + ethnicity | demo.group,
    data = demo.dt,
    overall = FALSE
  )
  
  return(demographic.table.html)
  
}
