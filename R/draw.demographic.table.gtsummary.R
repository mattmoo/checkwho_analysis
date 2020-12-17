##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param pre.post.figure.dt
##' @param time.series.figure.dt
draw.demographic.table.gtsummary <- function(pre.post.figure.dt,
                                             time.series.figure.dt) {
  
  pre.post.figure.dt[SSC == 'Pre', demo.group := 'Pre-SSC']
  pre.post.figure.dt[SSC == 'Post', demo.group := 'Post-SSC']
  time.series.figure.dt[, demo.group := 'Time-series']
  time.series.figure.dt[, demo.group := factor(demo.group,
                                               levels = c('Pre-SSC',
                                                          'Post-SSC',
                                                          'Time-series'))]
  
  demo.dt = rbindlist(list(pre.post.figure.dt,
                           time.series.figure.dt))
  
  rename.list = list(
    gender ~ 'Gender',
    age.group ~ 'Age',
    ethnicity ~ 'Ethnic group',
    acuity ~ 'ASA Acuity',
    asa.status ~ 'ASA Physical Status',
    icd.chapter.grouped ~ 'Procedure type'
  )
  
  pre.post.input.dt = droplevels(demo.dt[demo.group %in% c('Pre-SSC', 'Post-SSC'),
                                         .(gender,
                                           age.group,
                                           ethnicity,
                                           acuity,
                                           asa.status,
                                           icd.chapter.grouped,
                                           demo.group)])
  time.series.input.dt = droplevels(time.series.figure.dt[, .(gender,
                                                              age.group,
                                                              ethnicity,
                                                              acuity,
                                                              asa.status,
                                                              icd.chapter.grouped,
                                                              demo.group)])
  time.series.input.dt[, demo.group := NULL]
  
  a =
    tbl_summary(pre.post.input.dt,
                by = 'demo.group',
                label = rename.list) %>%
    add_p(test = list(age.group ~ 'chisq.test'))
  
  b =
    tbl_summary(time.series.input.dt,
                label = rename.list)
  
  demographic.table.gtsummary = tbl_merge(list(b, a),
                                          tab_spanner = c("**Extended**", "**Pre/Post**"))
  
  # Adjust footnotes
  demographic.table.gtsummary$table_header$footnote[demographic.table.gtsummary$table_header$footnote == "Statistics presented: n (%)"] = NA
  demographic.table.gtsummary$table_header$footnote = stringr::str_replace(
    demographic.table.gtsummary$table_header$footnote,
    pattern = 'Statistical tests performed: ',
    replacement = 'Pre-SSC Period and Post-SSC Period compared using '
  )
  
  return(demographic.table.gtsummary)
  
}
