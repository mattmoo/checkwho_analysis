##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param adhb.revision.one.recalculated.event.op.patient.dt
clean.adhb.revision.one.recalculated.op.dt <- function(adhb.revision.one.recalculated.event.op.patient.dt) {

  
  adhb.revision.one.recalculated.op.dt = adhb.revision.one.recalculated.event.op.patient.dt[,
                                        c(
                                          "Theatre Event ID",
                                          "Event ID",
                                          "Proc 1 Code",
                                          "Proc 1 Desc",
                                          "Proc 2 Code",
                                          "Proc 2 Desc",
                                          "Proc 3 Code",
                                          "Proc 3 Desc",
                                          "Proc 4 Code",
                                          "Proc 4 Desc",
                                          "Proc 5 Code",
                                          "Proc 5 Desc",
                                          "Proc 6 Code",
                                          "Proc 6 Desc",
                                          "Proc 7 Code",
                                          "Proc 7 Desc",
                                          "Proc 8 Code",
                                          "Proc 8 desc",
                                          "Proc 9 Code",
                                          "Proc 9 Desc",
                                          "Proc 10 Code",
                                          "Proc 10 Desc"
                                        ),
                                        with = FALSE]
  
  setnames(
    adhb.revision.one.recalculated.op.dt,
    old = c(
      "Proc 1 Code",
      "Proc 1 Desc",
      "Proc 2 Code",
      "Proc 2 Desc",
      "Proc 3 Code",
      "Proc 3 Desc",
      "Proc 4 Code",
      "Proc 4 Desc",
      "Proc 5 Code",
      "Proc 5 Desc",
      "Proc 6 Code",
      "Proc 6 Desc",
      "Proc 7 Code",
      "Proc 7 Desc",
      "Proc 8 Code",
      "Proc 8 desc",
      "Proc 9 Code",
      "Proc 9 Desc",
      "Proc 10 Code",
      "Proc 10 Desc"
    ),
    new = c(
      "Proc Code 1",
      "Proc Desc 1",
      "Proc Code 2",
      "Proc Desc 2",
      "Proc Code 3",
      "Proc Desc 3",
      "Proc Code 4",
      "Proc Desc 4",
      "Proc Code 5",
      "Proc Desc 5",
      "Proc Code 6",
      "Proc Desc 6",
      "Proc Code 7",
      "Proc Desc 7",
      "Proc Code 8",
      "Proc Desc 8",
      "Proc Code 9",
      "Proc Desc 9",
      "Proc Code 10",
      "Proc Desc 10"
    )
  )
  
  adhb.revision.one.recalculated.op.dt = melt(adhb.revision.one.recalculated.op.dt,
                    measure = patterns("^Proc Code ", "^Proc Desc "),
                    value.name = c("op.code", "op.desc"),
                    variable.name = 'op.number')[!is.na(op.code)]
  
  adhb.revision.one.recalculated.op.dt = unique(adhb.revision.one.recalculated.op.dt)
  
  setorder(adhb.revision.one.recalculated.op.dt, `Event ID`, op.number) 
  
  return(adhb.revision.one.recalculated.op.dt)

}
