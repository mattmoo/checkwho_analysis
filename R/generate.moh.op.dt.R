##' Pulls out operations from the diagnoses, and attach descriptions.
##'
##' .. content for \details{} ..
##'
##' @title
##' @param moh.diag.op.dt Cleaned diagnoses data.table
##' @param op.lookup.dt Lookup table with data on operation codes.
generate.moh.op.dt <- function(moh.diag.op.dt, op.lookup.dt) {

  moh.op.dt = moh.diag.op.dt[DIAG_TYP == 'O']
  
  # moh.op.dt[, CLIN_CD := as.numeric(CLIN_CD)]
  
  moh.op.dt = merge(x = moh.op.dt,
                    y = op.lookup.dt,
                    by.x = c("CLIN_SYS", "CLIN_CD","DIAG_TYP"),
                    by.y = c("CLIN_SYS", "clinical_code","clinical_code_type"),
                    all.x = TRUE)
  
  setnames(
    moh.op.dt,
    old = c(
      "chapter_code",
      "clinical_code_description",
      "block_code",
      "chapter_description"
    ),
    new = c(
      "code.chapter",
      "desc.procedure",
      "code.block",
      "desc.chapter"
    )
  )
  
  moh.op.dt[, desc.chapter :=
              gsub(
                pattern = ' (forward mapped CLIN_SYS 13)',
                replacement = '',
                x = desc.chapter,
                fixed = TRUE
              )]
  
  return(moh.op.dt)

}
