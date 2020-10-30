##' Generate a table with variables for the eligibility
##'
##' Assesses eligibility of each ADHB event to be included in the study.
##'
##' @title
##' @param event.opdate.dt Merged ADHB and MOH events
##' @param adhb.op.dt ADHB operations data.table
##' @param adhb.older.version.event.op.patient.raw.dt Initial data from Revision
##'   1, to include the exact same patients in the pre vs post
##' @param figureDaohData Final data from Revision 1, to include the exact same
##'   patients in the pre vs post
generate.eligibility.dt <- function(event.opdate.dt, 
                                    adhb.op.dt, 
                                    moh.patient.dt,
                                    moh.op.dt,
                                    min.date, 
                                    max.date, 
                                    pre.period.start,
                                    pre.period.end, 
                                    post.period.start,
                                    post.period.end
                                    # adhb.older.version.event.op.patient.raw.dt,
                                    # figureDaohData
                                    ) {
  
  # New table with just IDs
  eligibility.dt = event.opdate.dt[, .(PRIM_HCU = PRIM_HCU,
                                       moh.event.id = EVENT_ID, 
                                       adhb.event.id = `Event ID`,
                                       # adhb.theatre.event.id = `Theatre Event ID`,
                                       # adhb.op.date.time = `Actual Into Theatre Date Time`,
                                       moh.admit.date = EVSTDATE,
                                       asa.status,
                                       moh.discharge.date = EVENDATE,
                                       # adhb.op.date = as.Date(`Actual Into Theatre Date Time`),
                                       moh.op.date = OP_ACDTE,
                                       ethnicity = ETHNICGP)]
  
  
  # # Flag in data set from Revision 1
  # setkey(eligibility.dt, moh.event.id) #Key is ADHB admission
  # eligibility.dt[, in.revision.1 := FALSE]
  # eligibility.dt[figureDaohData[,
  #                               .(adhb.event.id = as.character(eventID))],
  #                in.revision.1 := TRUE]
  
  # Flag event match successful.
  setkey(eligibility.dt, adhb.event.id) #Key is ADHB admission
  eligibility.dt[, unique.moh.event.opdate := FALSE]
  eligibility.dt[event.opdate.dt[!is.na(PRIM_HCU), 
                                 .(adhb.event.id = `Event ID`)],
                 unique.moh.event.opdate := TRUE]
  
  #Flag patient match successful
  setkey(eligibility.dt, PRIM_HCU) #Key is NHI
  eligibility.dt = merge(x = eligibility.dt, 
                         y = moh.patient.dt[,.(PRIM_HCU, 
                                               date_of_birth, 
                                               date_of_death)], 
                         by = 'PRIM_HCU', 
                         all.x = TRUE)
  eligibility.dt[, matched.moh.patient := FALSE]
  eligibility.dt[!is.na(date_of_birth),
                 matched.moh.patient := TRUE]
  
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
  
  # Flag first admission in each period per period, per patient.
  setkey(eligibility.dt, moh.event.id, moh.op.date)
  eligibility.dt[eligibility.dt[in.pre.period == TRUE,.SD[moh.op.date == min(moh.op.date), .(moh.event.id, moh.op.date)], by = PRIM_HCU][,.(moh.event.id, moh.op.date)],
                 in.pre.period.first := TRUE]
  eligibility.dt[eligibility.dt[in.post.period == TRUE,.SD[moh.op.date == min(moh.op.date), .(moh.event.id, moh.op.date)], by = PRIM_HCU][,.(moh.event.id, moh.op.date)],
                 in.post.period.first := TRUE]
  
  # Flag younger than 16
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
  
  # Flag event/opdates with at least one procedure that is eligible.
  setkey(eligibility.dt, moh.event.id, moh.op.date) #Key is now theatre event
  eligibility.dt = merge(x = eligibility.dt,
                         y = event.opdate.dt[!is.na(EVENT_ID), .(
                           moh.event.id = EVENT_ID,
                           moh.op.date = OP_ACDTE,
                           eligible.procedure = !is.na(desc.chapter)
                         )], 
                         by = c('moh.event.id', 'moh.op.date'),
                         all.x = TRUE)
  
  # Flag ADHB operation dates that do not overlap the MOH admission dates
  eligibility.dt[, overlapping.op.and.admission := moh.op.date >= moh.admit.date & moh.op.date <= moh.discharge.date]
  
  # # Flag first theatre event of each admission
  # setkey(eligibility.dt, adhb.theatre.event.id) #Key is now theatre event
  # eligibility.dt[, first.operation.adhb := FALSE]
  # eligibility.dt[eligibility.dt[has.operation.adhb == TRUE,.SD[1],by = adhb.event.id][,.(adhb.theatre.event.id)],
  #                first.operation.adhb := TRUE]
  
  # Flag no ASA
  eligibility.dt[, has.asa := !is.na(asa.status)]
  
  # Flag ASA 6
  eligibility.dt[, not.asa.6 := asa.status != 'ASA 6']
  
  eligibility.dt[,eligible.without.op := 
                   unique.moh.event.opdate & 
                   matched.moh.patient &
                   after.min.date &
                   before.max.date &
                   age.older.than.16 &
                   recorded.ethnicity &
                   alive.on.op.date &
                   # has.operation.adhb &
                   has.operation.moh &
                   overlapping.op.and.admission &
                   # first.operation.adhb &
                   has.asa & 
                   not.asa.6]
  
  eligibility.dt[,eligible := 
                   eligible.without.op &
                   eligible.procedure]
  
  # Flag unique patients by shuffling, then selecting first instance of patient to get a random admission.
  eligibility.dt[, time.series.eligible.and.unique := FALSE]
  eligibility.dt[eligibility.dt[eligible == TRUE,.SD[sample(.N)][1, .(moh.event.id, moh.op.date)], by = PRIM_HCU][,.(moh.event.id, moh.op.date)],
                 time.series.eligible.and.unique := TRUE]
  
  eligibility.dt[, pre.post.unique := FALSE]
  # eligibility.dt[eligibility.dt[eligible & (in.pre.period | in.post.period) == TRUE,.SD[1, .(moh.event.id, moh.op.date)], by = PRIM_HCU][,.(moh.event.id, moh.op.date)],
  eligibility.dt[eligibility.dt[eligible & (in.pre.period | in.post.period) == TRUE,.SD[sample(.N)][1, .(moh.event.id, moh.op.date)], by = PRIM_HCU][,.(moh.event.id, moh.op.date)],
                                pre.post.unique := TRUE]

  

  eligibility.dt[, pre.eligible.and.unique := FALSE]
  eligibility.dt[eligibility.dt[pre.post.unique & in.pre.period == TRUE,.(moh.event.id, moh.op.date)],
                 pre.eligible.and.unique := TRUE]
  
  eligibility.dt[, post.eligible.and.unique := FALSE]
  eligibility.dt[eligibility.dt[pre.post.unique & in.post.period == TRUE,.(moh.event.id, moh.op.date)],
                 post.eligible.and.unique := TRUE]
  
  
  return(eligibility.dt)
}