##' Generate a table with variables for the eligibility
##'
##' Assesses eligibility of each ADHB event to be included in the study.
##'
##' @title
##' @param event.opdate.dt Merged ADHB and MOH events
##' @param adhb.theatre.event.dt ADHB theatre events data.table
##' @param random.pre.post The first revision had a wee issue where all of the
##'   events were the first per patient per period. If this is FALSE, replicate
##'   that. If TRUE, get a random event instead (as per the time series).
generate.eligibility.dt <- function(event.opdate.dt, 
                                    adhb.theatre.event.dt, 
                                    moh.patient.dt,
                                    moh.op.dt,
                                    min.date, 
                                    max.date, 
                                    pre.period.start,
                                    pre.period.end, 
                                    post.period.start,
                                    post.period.end,
                                    random.pre.post
                                    # adhb.older.version.event.op.patient.raw.dt,
                                    # figureDaohData
                                    ) {
  
  # # New table with just IDs
  # eligibility.dt = event.opdate.dt[, .(PRIM_HCU = PRIM_HCU,
  #                                      moh.event.id = EVENT_ID, 
  #                                      adhb.event.id = `Event ID`,
  #                                      # adhb.theatre.event.id = `Theatre Event ID`,
  #                                      # adhb.op.date.time = `Actual Into Theatre Date Time`,
  #                                      moh.admit.date = EVSTDATE,
  #                                      asa.status,
  #                                      moh.discharge.date = EVENDATE,
  #                                      max.block.clinical.severity,
  #                                      # adhb.op.date = as.Date(`Actual Into Theatre Date Time`),
  #                                      moh.op.date = OP_ACDTE,
  #                                      ethnicity = ETHNICGP)]
  
  eligibility.dt = merge(
    x = adhb.theatre.event.dt[`Actual Into Theatre Date Time` >= min.date &
                                `Actual Into Theatre Date Time` <= max.date, .(
      adhb.theatre.event.id = `Theatre Event ID`,
      adhb.event.id = `Event ID`,
      adhb.op.datetime = `Actual Into Theatre Date Time`,
      adhb.op.date = `Actual Into Theatre Date`
      
    )],
    y = event.opdate.dt[, .(
      PRIM_HCU = PRIM_HCU,
      moh.event.id = EVENT_ID,
      adhb.event.id = `Event ID`,
      moh.admit.date = EVSTDATE,
      asa.status,
      moh.discharge.date = EVENDATE,
      max.block.clinical.severity.grouped,
      moh.op.date = OP_ACDTE,
      ethnicity = ETHNICGP,
      all.known.asa = !any.unknown.asa
    )],
    by.x = c('adhb.event.id', "adhb.op.date"),
    by.y = c('adhb.event.id', "moh.op.date"),
    all.x = TRUE
  )
  setnames(eligibility.dt, "adhb.op.date", "moh.op.date")
  
  # # Flag in data set from Revision 1
  # setkey(eligibility.dt, moh.event.id) #Key is ADHB admission
  # eligibility.dt[, in.revision.1 := FALSE]
  # eligibility.dt[figureDaohData[,
  #                               .(adhb.event.id = as.character(eventID))],
  #                in.revision.1 := TRUE]
  
  # Flag event match successful.
  setkey(eligibility.dt, adhb.theatre.event.id) #Key is ADHB admission
  eligibility.dt[, moh.event.opdate.matched := FALSE]
  eligibility.dt[!is.na(moh.event.id), 
                 moh.event.opdate.matched := TRUE]

  # # Flag event opdate combos with a close match in date (+/-1 day) in the ADHB theatre events.
  # eligibility.dt[, match.adhb.theatre.event.date := FALSE]
  # event.opdate.theatre.event.dt = merge(
  #   x = event.opdate.dt[, .(`Event ID`, OP_ACDTE)],
  #   y = adhb.theatre.event.dt[, .(`Event ID`, `Actual Into Theatre Date`)],
  #   by = 'Event ID',
  #   all.x = TRUE,
  #   allow.cartesian = TRUE
  # )
  # event.opdate.theatre.event.dt[, diff.days := interval(OP_ACDTE, `Actual Into Theatre Date`) / days(1)]
  # event.opdate.theatre.event.dt = event.opdate.theatre.event.dt[, .(min.abs.date.diff = min(abs(diff.days))), by = .(`Event ID`, OP_ACDTE)]
  # eligibility.dt = merge(x = eligibility.dt,
  #                        y = event.opdate.theatre.event.dt,
  #                        by.x = c("adhb.event.id", "moh.op.date"),
  #                        by.y = c("Event ID", "OP_ACDTE"),
  #                        all.x = TRUE)
  # eligibility.dt[, match.adhb.theatre.event.date := min.abs.date.diff<2]
  
  #Flag patient match successful
  # setkey(eligibility.dt, PRIM_HCU) #Key is NHI
  eligibility.dt = merge(x = eligibility.dt,
                         y = moh.patient.dt[,.(PRIM_HCU,
                                               date_of_birth,
                                               date_of_death)],
                         by = 'PRIM_HCU',
                         all.x = TRUE)
  eligibility.dt[, matched.moh.patient := FALSE]
  eligibility.dt[!is.na(date_of_birth),
                 matched.moh.patient := TRUE]
  
  # Attach mortality for checking effect of criteria.
  eligibility.dt[,mort.30.day := interval(moh.op.date, date_of_death)/days(1) < 30]
  eligibility.dt[,mort.90.day := interval(moh.op.date, date_of_death)/days(1) < 90]
  
  # Flag cases after minimum date.
  eligibility.dt[,after.min.date := FALSE]
  eligibility.dt[moh.op.date >= min.date,
                 after.min.date := TRUE]
  eligibility.dt[,before.max.date := FALSE]
  eligibility.dt[moh.op.date < max.date,
                 before.max.date := TRUE]
  
  # Flag before/after periods.
  eligibility.dt[,in.pre.period := FALSE]
  eligibility.dt[moh.op.date >= pre.period.start & moh.op.date < pre.period.end,
                 in.pre.period := TRUE]
  eligibility.dt[,in.post.period := FALSE]
  eligibility.dt[moh.op.date >= post.period.start & moh.op.date < post.period.end,
                 in.post.period := TRUE]
  eligibility.dt[in.pre.period == TRUE, pre.post.period := 'Pre']
  eligibility.dt[in.post.period == TRUE, pre.post.period := 'Post']
  
  # Flag first admission in each period per period, per patient.
  setkey(eligibility.dt, adhb.theatre.event.id)
  eligibility.dt[, in.pre.period.first := FALSE]
  eligibility.dt[eligibility.dt[in.pre.period == TRUE,
                                .SD[adhb.op.datetime == min(adhb.op.datetime),
                                    .(adhb.theatre.event.id)][1],
                                by = PRIM_HCU][!is.na(PRIM_HCU), .(adhb.theatre.event.id)],
                 in.pre.period.first := TRUE]
  eligibility.dt[, in.post.period.first := FALSE]
  eligibility.dt[eligibility.dt[in.post.period == TRUE,
                                .SD[adhb.op.datetime == min(adhb.op.datetime),
                                    .(adhb.theatre.event.id)][1],
                                by = PRIM_HCU][!is.na(PRIM_HCU), .(adhb.theatre.event.id)],
                 in.post.period.first := TRUE]
  
  # Flag younger than 16
  setkey(eligibility.dt, moh.event.id, moh.op.date)
  eligibility.dt[,
                 age := interval(date_of_birth, moh.op.date)/years(1)]
  eligibility.dt[, age.older.than.16 := age >= 16]
  
  # Flag ethnicity present
  eligibility.dt[, recorded.ethnicity := !is.na(ethnicity)]
  
  # Flag death before op date
  eligibility.dt[, alive.on.op.date := is.na(date_of_death) | (date_of_death >= moh.op.date)]
  
  # Flag theatre events with a recorded procedure from MOH
  setkey(eligibility.dt, moh.event.id) #Key is now MOH event
  eligibility.dt[, has.operation.moh := FALSE]
  eligibility.dt[unique(moh.op.dt[, .(moh.event.id = as.character(EVENT_ID))]),
                 has.operation.moh := TRUE]
  # 
  # # Flag theatre events with a recorded procedure from ADHB
  # setkey(eligibility.dt, adhb.theatre.event.id) #Key is now theatre event
  # eligibility.dt[, has.operation.adhb := FALSE]
  # eligibility.dt[adhb.theatre.event.dt[`Primary Proc` != '', .(`Theatre Event ID`)],
  #                has.operation.adhb := TRUE]
  # 
  
  # # Flag event/opdates with at least one procedure that is eligible.
  # setkey(eligibility.dt, moh.event.id, moh.op.date) #Key is now theatre event
  # eligibility.dt = merge(x = eligibility.dt,
  #                        y = event.opdate.dt[!is.na(EVENT_ID), .(
  #                          moh.event.id = EVENT_ID,
  #                          moh.op.date = OP_ACDTE,
  #                          eligible.procedure = !is.na(desc.chapter)
  #                        )], 
  #                        by = c('moh.event.id', 'moh.op.date'),
  #                        all.x = TRUE)
  # # Flag event/opdates with at least one procedure that is an operation.
  eligibility.dt[, eligible.procedure := max.block.clinical.severity.grouped != 0]
  
  # Flag ADHB operation dates that do not overlap the MOH admission dates
  eligibility.dt[, overlapping.op.and.admission := moh.op.date >= moh.admit.date & moh.op.date <= moh.discharge.date]
  
  # Flag no ASA
  eligibility.dt[, has.asa := !is.na(asa.status)]
  
  # Flag ASA 6
  eligibility.dt[, not.asa.6 := is.na(asa.status) | asa.status != 'ASA 6']
  
  # Flag donor operation
  eligibility.dt[, no.donor.operation := max.block.clinical.severity.grouped != 999]
  
  eligibility.dt[,eligible := 
                   moh.event.opdate.matched & 
                   matched.moh.patient &
                   after.min.date &
                   before.max.date &
                   age.older.than.16 &
                   # match.adhb.theatre.event.date &
                   recorded.ethnicity &
                   alive.on.op.date &
                   # has.operation.adhb &
                   has.operation.moh &
                   overlapping.op.and.admission &
                   eligible.procedure &
                   # first.operation.adhb &
                   # has.asa & 
                   not.asa.6 &
                   no.donor.operation]
  
  
  # Flag first eligible theatre event of each admission
  setkey(eligibility.dt, adhb.theatre.event.id) #Key is now theatre event
  eligibility.dt[, first.eligible.operation.adhb := FALSE]
  eligibility.dt[eligibility.dt[eligible == TRUE,.SD[1],by = adhb.event.id][,.(adhb.theatre.event.id)],
                 first.eligible.operation.adhb := TRUE]
  
  eligibility.dt[,eligible :=
                   eligible &
                   first.eligible.operation.adhb]
  
  # Flag unique patients by shuffling, then selecting first instance of patient to get a random admission.
  setkey(eligibility.dt, adhb.theatre.event.id)
  eligibility.dt[, time.series.eligible.and.unique := FALSE]
  eligibility.dt[eligibility.dt[eligible == TRUE,.SD[sample(.N)][1, .(adhb.theatre.event.id)], by = PRIM_HCU][,.(adhb.theatre.event.id)],
                 time.series.eligible.and.unique := TRUE]
  
  if (random.pre.post == TRUE) {
    # Similarly get random for pre/post
    eligibility.dt[, pre.post.eligible.and.unique := FALSE]
    eligibility.dt[eligibility.dt[eligible &
                                    (in.pre.period |
                                       in.post.period) == TRUE,
                                  .SD[sample(.N)][1, .(adhb.theatre.event.id)],
                                  by = PRIM_HCU][, .(adhb.theatre.event.id)],
                   pre.post.eligible.and.unique := TRUE]
    
  } else {
    # Or actually just choose one of the first eligible from each period,
    # consistent with first revision.
    eligibility.dt[, both.pre.post.eligible.first := FALSE]
    eligibility.dt[eligibility.dt[(eligible &
                                     (in.pre.period | in.post.period)) == TRUE,
                                  .SD[1, .(adhb.theatre.event.id)],
                                  by = .(PRIM_HCU, pre.post.period)][,.(adhb.theatre.event.id)],
                   both.pre.post.eligible.first := TRUE]
    eligibility.dt[, pre.post.eligible.and.unique := FALSE]
    eligibility.dt[eligibility.dt[both.pre.post.eligible.first == TRUE,
                                  .SD[sample(.N)][1, .(adhb.theatre.event.id)],
                                  by = PRIM_HCU][, .(adhb.theatre.event.id)],
                   pre.post.eligible.and.unique := TRUE]
    
    # Only include operations that were first, and admissions with all known ASAs.
    # eligibility.dt[, pre.post.eligible.and.unique := FALSE]
    # eligibility.dt[eligibility.dt[eligible & all.known.asa &
    #                                 (in.pre.period.first | in.post.period.first) == TRUE,
    #                               .SD[sample(.N)][1, .(adhb.theatre.event.id)],
    #                               by = PRIM_HCU][, .(adhb.theatre.event.id)],
    #                pre.post.eligible.and.unique := TRUE]
    
  }

  

  eligibility.dt[, pre.eligible.and.unique := FALSE]
  eligibility.dt[eligibility.dt[pre.post.eligible.and.unique & in.pre.period == TRUE,.(adhb.theatre.event.id)],
                 pre.eligible.and.unique := TRUE]
  
  eligibility.dt[, post.eligible.and.unique := FALSE]
  eligibility.dt[eligibility.dt[pre.post.eligible.and.unique & in.post.period == TRUE,.(adhb.theatre.event.id)],
                 post.eligible.and.unique := TRUE]
  
  
  return(eligibility.dt)
}