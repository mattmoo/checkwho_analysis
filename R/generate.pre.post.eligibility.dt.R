##' Generate a table with variables for the eligibility
##'
##' Assesses eligibility of each ADHB event to be included in the study.
##'
##' @title
##' @param event.opdate.dt Merged ADHB and MOH events
##' @param adhb.op.dt ADHB operations data.table
##' @param adhb.revision.one.event.dt Initial data from Revision
##'   1, to include the exact same patients in the pre vs post
##' @param figureDaohData Final data from Revision 1, to include the exact same
##'   patients in the pre vs post
generate.pre.post.eligibility.dt <-
  function(revision.one.recalculated.event.opdate.dt,
           adhb.op.dt,
           moh.patient.dt,
           moh.op.dt,
           min.date,
           max.date,
           pre.period.start,
           pre.period.end,
           post.period.start,
           post.period.end,
           # adhb.revision.one.event.dt,
           figureDaohData
           ) {
    
  
  # New table with just IDs
  pre.post.eligibility.dt = revision.one.recalculated.event.opdate.dt[, .(
    PRIM_HCU = NHI,
    moh.event.id = EVENT_ID,
    adhb.event.id = `Event ID`,
    # adhb.theatre.event.id = `Theatre Event ID`,
    # adhb.op.date.time = `Actual Into Theatre Date Time`,
    moh.admit.date = EVSTDATE,
    asa.status,
    moh.discharge.date = EVENDATE,
    # adhb.op.date = as.Date(`Actual Into Theatre Date Time`),
    moh.op.date = OP_ACDTE,
    ethnicity = ETHNICGP
  )]
  

  # Flag in data set from Revision 1
  setkey(pre.post.eligibility.dt, moh.event.id) #Key is MOH admission
  pre.post.eligibility.dt[, in.revision.one.event := FALSE]
  pre.post.eligibility.dt[figureDaohData[,
                                         .(moh.event.id = as.character(eventID))],
                          in.revision.one.event := TRUE]
  
  setkey(pre.post.eligibility.dt, moh.event.id, moh.op.date) #Key is MOH admission and opdate
  pre.post.eligibility.dt[, in.revision.one.event.opdate := FALSE]
  pre.post.eligibility.dt[figureDaohData[,
                                         .(moh.event.id = as.character(eventID),
                                           moh.op.date = daohStartDate)],
                          in.revision.one.event.opdate := TRUE]
  
  
  
  # Flag event match successful.
  setkey(pre.post.eligibility.dt, adhb.event.id) #Key is ADHB admission
  pre.post.eligibility.dt[, unique.moh.event.opdate := FALSE]
  pre.post.eligibility.dt[revision.one.recalculated.event.opdate.dt[!is.na(EVENT_ID),
                                                                    .(adhb.event.id = `Event ID`)],
                          unique.moh.event.opdate := TRUE]
  
  
  #Flag patient match successful
  setkey(pre.post.eligibility.dt, PRIM_HCU) #Key is NHI
  pre.post.eligibility.dt = merge(
    x = pre.post.eligibility.dt,
    y = moh.patient.dt[, .(PRIM_HCU,
                           date_of_birth,
                           date_of_death)],
    by = 'PRIM_HCU',
    all.x = TRUE
  )
  pre.post.eligibility.dt[, matched.moh.patient := FALSE]
  pre.post.eligibility.dt[!is.na(date_of_birth),
                          matched.moh.patient := TRUE]
  
  
  # # Flag cases after minimum date.
  # pre.post.eligibility.dt[, after.min.date := FALSE]
  # pre.post.eligibility.dt[moh.op.date >= min.date,
  #                         after.min.date := TRUE]
  # pre.post.eligibility.dt[, before.max.date := FALSE]
  # pre.post.eligibility.dt[moh.op.date <= max.date,
  #                         before.max.date := TRUE]
  
  # Flag before/after periods.
  pre.post.eligibility.dt[, in.pre.period := FALSE]
  pre.post.eligibility.dt[moh.op.date >= pre.period.start &
                            moh.op.date <= pre.period.end,
                          in.pre.period := TRUE]
  pre.post.eligibility.dt[, in.post.period := FALSE]
  pre.post.eligibility.dt[moh.op.date >= post.period.start &
                            moh.op.date <= post.period.end,
                          in.post.period := TRUE]
  
  
  # Flag younger than 16
  pre.post.eligibility.dt[,
                          age := interval(date_of_birth, moh.op.date) / years(1)]
  pre.post.eligibility.dt[, age.older.than.16 := age >= 16]
  
  # Flag ethnicity present
  pre.post.eligibility.dt[, recorded.ethnicity := !is.na(ethnicity)]
  
  # Flag death before op date
  pre.post.eligibility.dt[, alive.on.op.date := is.na(date_of_death) |
                            (date_of_death >= moh.op.date)]
  
  # Flag theatre events with a recorded procedure from MOH
  setkey(pre.post.eligibility.dt, moh.event.id) #Key is now MOH event
  pre.post.eligibility.dt[, has.operation.moh := FALSE]
  pre.post.eligibility.dt[unique(moh.op.dt[, .(moh.event.id = as.character(EVENT_ID))]),
                          has.operation.moh := TRUE]
  #
  # # Flag theatre events with a recorded procedure from ADHB
  # setkey(pre.post.eligibility.dt, adhb.theatre.event.id) #Key is now theatre event
  # pre.post.eligibility.dt[, has.operation.adhb := FALSE]
  # pre.post.eligibility.dt[adhb.theatre.event.dt[`Primary Proc` != '', .(`Theatre Event ID`)],
  #                has.operation.adhb := TRUE]
  #
  
  # # Flag event/opdates with at least one procedure that is eligible.
  # setkey(pre.post.eligibility.dt, moh.event.id, moh.op.date) #Key is now theatre event
  # pre.post.eligibility.dt = merge(
  #   x = pre.post.eligibility.dt,
  #   y = revision.one.recalculated.event.opdate.dt[!is.na(EVENT_ID), .(
  #     moh.event.id = EVENT_ID,
  #     moh.op.date = OP_ACDTE,
  #     eligible.procedure = !is.na(desc.chapter)
  #   )],
  #   by = c('moh.event.id', 'moh.op.date'),
  #   all.x = TRUE
  # )
  
  # Flag ADHB operation dates that do not overlap the MOH admission dates
  pre.post.eligibility.dt[, overlapping.op.and.admission := moh.op.date >= moh.admit.date &
                            moh.op.date <= moh.discharge.date]
  
  # # Flag first theatre event of each admission
  # setkey(pre.post.eligibility.dt, adhb.theatre.event.id) #Key is now theatre event
  # pre.post.eligibility.dt[, first.operation.adhb := FALSE]
  # pre.post.eligibility.dt[pre.post.eligibility.dt[has.operation.adhb == TRUE,.SD[1],by = adhb.event.id][,.(adhb.theatre.event.id)],
  #                first.operation.adhb := TRUE]
  
  # Flag no ASA
  pre.post.eligibility.dt[, has.asa := !is.na(asa.status)]
  
  
  # Flag ASA 6
  pre.post.eligibility.dt[, not.asa.6 := asa.status != 'ASA 6']
  
  
  pre.post.eligibility.dt[, eligible :=
                            unique.moh.event.opdate &
                            matched.moh.patient &
                            # after.min.date &
                            # before.max.date &
                            age.older.than.16 &
                            recorded.ethnicity &
                            alive.on.op.date &
                            # has.operation.adhb &
                            has.operation.moh &
                            overlapping.op.and.admission &
                            # first.operation.adhb &
                            has.asa &
                            not.asa.6]
  
  
  # Get a random event for those with more than one (should be at most two)
  setkey(pre.post.eligibility.dt, moh.event.id, moh.op.date)
  pre.post.eligibility.dt[, pre.post.unique := FALSE]
  pre.post.eligibility.dt[pre.post.eligibility.dt[eligible &
                                                    (in.pre.period |
                                                       in.post.period) == TRUE, .SD[sample(.N)][1, .(moh.event.id, moh.op.date)], by = PRIM_HCU][, .(moh.event.id, moh.op.date)],
                          pre.post.unique := TRUE]
  
  
  pre.post.eligibility.dt[, pre.eligible.and.unique := FALSE]
  pre.post.eligibility.dt[pre.post.eligibility.dt[pre.post.unique &
                                                    in.pre.period == TRUE, .(moh.event.id, moh.op.date)],
                          pre.eligible.and.unique := TRUE]
  
  pre.post.eligibility.dt[, post.eligible.and.unique := FALSE]
  pre.post.eligibility.dt[pre.post.eligibility.dt[pre.post.unique &
                                                    in.post.period == TRUE, .(moh.event.id, moh.op.date)],
                          post.eligible.and.unique := TRUE]
  
  
  return(pre.post.eligibility.dt)
}