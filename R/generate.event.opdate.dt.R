##' Merges ADHB (i.e. ACH L8) events to the equivalent MOH events
##'
##' Unmatched events are retained.
##'
##' @title
##' @param adhb.event.dt Cleaned ADHB event data.table
##' @param moh.event.dt Cleaned MOH event data.table
##' @param moh.op.dt Cleaned MOH operations data.table
##' @param asa.dt Data.table of ASA pulled from MOH operation codes
##' @param facility.lookup.dt Data.table of NZ healthcare facilities
##' @param ethnicity.lookup.dt Data.table of ethnicity codes as defined by MOH
generate.event.opdate.dt <- function(adhb.event.dt, 
                                     moh.event.dt,
                                     moh.op.dt,
                                     asa.dt,
                                     facility.lookup.dt,
                                     ethnicity.lookup.dt,
                                     max.clin.sev.dt,
                                     min.clin.sev.dt
                                     ) {

  # Attach facility name and DHB (not strictly necessary, could do it by code)
  moh.event.dt = merge(x = moh.event.dt,
                       y = facility.lookup.dt[,.(FACILITY = `Health Facility Code`, 
                                                 Facility.name = Name,
                                                 DHB = `DHB Name`,
                                                 Facility.type = `Facility Type Name`)],
                       by = 'FACILITY',
                       all.x = TRUE)
  
  # # Combine ADHB events with those found by the last paper.,
  # adhb.event.dt = unique(rbindlist(list(
  #   adhb.event.dt[,.(`Event ID`, NHI)],
  #   adhb.revision.one.recalculated.event.dt[!`Event ID` %in% adhb.event.dt[,`Event ID`]]
  # ),
  # fill = TRUE))
  # adhb.event.dt[, revision.one := `Event ID` %in% adhb.revision.one.recalculated.event.dt[,`Event ID`]]
  
  
  # Print codes of facilities with no entry in lookup table.
  missing.facility.dt = moh.event.dt[is.na(Facility.name), .N, by = FACILITY]
  if (nrow(missing.facility.dt) > 0) {
    message('Missing facility names:')
    print(missing.facility.dt)
  }
  
  # Merge on the basis of PMS identifier
  merged.event.dt = merge(
    x = adhb.event.dt,
    y = moh.event.dt[DHB == 'Auckland District Health Board'],
    by.x = 'Event ID',
    by.y = 'PMS_UNIQUE_IDENTIFIER',
    all.x = TRUE
  )
  
  # Attach priority ethnicity
  merged.event.dt = merge(
    x = merged.event.dt,
    y = unique(ethnicity.lookup.dt[, .(code.L2,
                                       ethnicg.desc.L1 = desc.L1,
                                       ethnicg.desc.L2 = desc.L2)]), 
    by.x = 'ETHNICGP',
    by.y = 'code.L2',
    all.x = TRUE
  )
  merged.event.dt[ETHNICGP == 54, ethnicg.desc.L1 := 'Other']
  merged.event.dt[ETHNICGP == 54, ethnicg.desc.L2 := 'Other']
  
  
  # Create rows for every unique event and operation date combination
  event.opdate.dt = merge(x = merged.event.dt,
                          y = unique(moh.op.dt[, .(EVENT_ID = as.character(EVENT_ID), OP_ACDTE)]),
                          by = 'EVENT_ID',
                          all.x = TRUE)
  
  # Attach minimum ASA per admission and op date, according to MOH.
  unique.asa.dt = asa.dt[!is.na(asa.status), .(asa.status = min(asa.status)), by = c('moh.event.id', 'moh.op.date')]
  unique.asa.dt = merge(unique.asa.dt,
                        unique(asa.dt[, .(moh.event.id, moh.op.date, asa.status, asa.acute)]
                        )[, .(asa.acuity = min(asa.acute)), 
                          by = c('moh.event.id', 'moh.op.date', 'asa.status')],
                        by = c('moh.event.id', 'moh.op.date', 'asa.status'))
  
  unique.asa.dt[,asa.acuity := factor(asa.acuity, 
                                      levels = c('Not acute (or unknown)', 'Acute'), 
                                      ordered = FALSE)]
  
  event.opdate.dt = merge(x = event.opdate.dt,
                          y = unique.asa.dt, 
                          by.x = c('EVENT_ID', 'OP_ACDTE'),
                          by.y = c('moh.event.id', 'moh.op.date'),
                          all.x = TRUE
  )
  
  # Attach unknown ASA scores, for replicating Revision 1 results where I
  # accidentally excluded them.
  setkey(event.opdate.dt, EVENT_ID, OP_ACDTE)
  event.opdate.dt[,any.unknown.asa := FALSE]
  event.opdate.dt[asa.dt[is.na(asa.status), .(moh.event.id, moh.op.date)],
                  any.unknown.asa := TRUE]
  
  
  # Attach acuity
  moh.event.dt[ADM_TYPE == 'AC', acuity := 'Acute']
  moh.event.dt[ADM_TYPE != 'AC', acuity := 'Not acute']
  
  moh.event.dt[, acuity := factor(
    acuity,
    levels = c('Not acute', 'Acute'),
    labels = c('Not acute', 'Acute')
  )]
  
  event.opdate.dt = merge(
    x = event.opdate.dt,
    y = moh.event.dt[,.(EVENT_ID, acuity)],
    by = c('EVENT_ID'),
    all.x = TRUE
  )
  
  # Attach the max block clinical severity per event opdate.
  event.opdate.dt = merge(
    x = event.opdate.dt,
    y = max.clin.sev.dt[, .(
      EVENT_ID = as.character(EVENT_ID),
      OP_ACDTE,
      code.chapter,
      desc.chapter,
      code.procedure,
      desc.procedure,
      max.block.clinical.severity
    )],
    # by.x = c("EVENT_ID", "Actual Into Theatre Date"),
    by = c("EVENT_ID", "OP_ACDTE"),
    all.x = TRUE
  )
  event.opdate.dt[, max.block.clinical.severity.grouped := max.block.clinical.severity]
  event.opdate.dt[max.block.clinical.severity %in% c(1,2), max.block.clinical.severity.grouped := '1-2']
  # event.opdate.dt[max.block.clinical.severity == 3, max.block.clinical.severity.grouped := '3']
  event.opdate.dt[max.block.clinical.severity %in% c(4,5), max.block.clinical.severity.grouped := '4-5']
  # event.opdate.dt[max.block.clinical.severity == 999, max.block.clinical.severity.grouped := '999']
  event.opdate.dt[, max.block.clinical.severity.grouped := factor(max.block.clinical.severity.grouped,
                                                                  levels = c('0', '1-2', '3', '4-5', '999'))]
  
  event.opdate.dt = merge(
    x = event.opdate.dt,
    y = min.clin.sev.dt[, .(
      EVENT_ID = as.character(EVENT_ID),
      OP_ACDTE,
      min.block.clinical.severity
    )],
    # by.x = c("EVENT_ID", "Actual Into Theatre Date"),
    by = c("EVENT_ID", "OP_ACDTE"),
    all.x = TRUE
  )
  
  
  
  return(event.opdate.dt)
  
}
