##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param moh.op.dt
generate.asa.dt <- function(moh.op.dt) {
  
  asa.acute.levels = list(
    '0' = 'Acute',
    '9' = 'Not acute (or unknown)'
  )
  
  asa.status.levels = list(
    '1' = 'ASA 1',
    '2' = 'ASA 2',
    '3' = 'ASA 3',
    '4' = 'ASA 4',
    '5' = 'ASA 5',
    '6' = 'ASA 6'
  )
  
  # Pull out only anaesthetic procedures with ASA.
  asa.dt = moh.op.dt[desc.procedure %like% ', ASA', .(CLIN_CD, moh.event.id = as.character(EVENT_ID), moh.op.date = OP_ACDTE, desc.procedure)]
  
  # Extract ASA (last two digits)
  asa.dt[, asa.acute := stringr::str_extract(desc.procedure, "[0,9]$")]
  asa.dt[, asa.status := stringr::str_extract(desc.procedure, "(\\d)(?=[0,9]$)")]
  
  # Create factors
  asa.dt[, asa.acute := ordered(asa.acute,
                               levels = names(asa.acute.levels),
                               labels = asa.acute.levels)]
  asa.dt[, asa.status := ordered(asa.status,
                                levels = names(asa.status.levels),
                                labels = asa.status.levels)]
  
  # There will be some duplicate rows (same ASA on the same day during same admission)
  asa.dt = unique(asa.dt)
  
  return(asa.dt)
}
