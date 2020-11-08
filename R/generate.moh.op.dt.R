##' Pulls out operations from the diagnoses, and attach descriptions.
##'
##' .. content for \details{} ..
##'
##' @title
##' @param moh.diag.op.dt Cleaned diagnoses data.table
##' @param op.lookup.dt Lookup table with data on operation codes.
##' @param op.forward.mapping.dt Forward mapping table to 8th, so that 
generate.moh.op.dt <- function(moh.diag.op.dt, 
                               op.lookup.dt, 
                               op.forward.mapping.dt) {

  moh.op.dt = moh.diag.op.dt[DIAG_TYP == 'O']
  
  # moh.op.dt[, CLIN_CD := as.numeric(CLIN_CD)]
  
  moh.op.dt = merge(x = moh.op.dt,
                    y = op.lookup.dt,
                    by.x = c("CLIN_SYS", "CLIN_CD","DIAG_TYP"),
                    by.y = c("CLIN_SYS", "clinical_code","clinical_code_type"),
                    all.x = TRUE)
  
  setnames(
    op.forward.mapping.dt,
    old = c(
      "CLIN_SYS_from",
      "clinical_code_from",
      "clinical_code_to"
    ),
    new = c(
      "CLIN_SYS",
      "clinical_code",
      "clinical_code_8th"
    )
  )
  
  # Stuff for adding clinical severity, needs to be 8th edition.
  orig.names = names(moh.op.dt)
  
  moh.op.dt = merge(
    x = moh.op.dt,
    y = op.forward.mapping.dt[, .(CLIN_SYS, 
                                  CLIN_CD = clinical_code, 
                                  clinical_code_8th)],
    by = c("CLIN_SYS", "CLIN_CD"),
    all.x = TRUE
  )
  
  moh.op.dt = merge(
    x = moh.op.dt,
    y = op.lookup.dt[CLIN_SYS == 14, .(clinical_code_8th = clinical_code,
                                       block_code_8th = block_code)],
    by = c("clinical_code_8th"),
    all.x = TRUE
  )
  
  
  moh.op.dt = merge(
    x = moh.op.dt,
    y = unique(icd10amachi::icd10achi.block.dt[CLIN_SYS == 14, .(block_code_8th = block_code,
                                                                 block.clinical.severity = clinical_severity,
                                                                 chapter_code_8th = chapter_code)]),
    by = c("block_code_8th"),
    all.x = TRUE
  )
  
  moh.op.dt = merge(
    x = moh.op.dt,
    y = unique(icd10amachi::icd10achi.chapter.dt[CLIN_SYS == 14, .(chapter_code_8th = chapter_code,
                                                                   chapter_description_8th = chapter_description)]),
    by = c("chapter_code_8th"),
    all.x = TRUE
  )
  
  
  moh.op.dt[is.na(block.clinical.severity), block.clinical.severity := 0]
  moh.op.dt[, block.clinical.severity := factor(block.clinical.severity,
                                               levels = c(0, 1, 2, 3, 4, 5, 999),
                                               ordered = TRUE)]
  
  setcolorder(moh.op.dt,
              neworder = orig.names)
  
  setnames(
    moh.op.dt,
    old = c(
      "chapter_code",
      "clinical_code_description",
      "block_code",
      "chapter_description_8th",
      "chapter_description"
    ),
    new = c(
      "code.chapter",
      "desc.procedure",
      "code.block",
      "desc.chapter",
      "chapter_description_3rd"
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
