##' Generates results in the same way as our original paper.
##'
##' @title
##' @param adhb.older.version.event.op.patient.dt
##' @param adhb.theatre.event.dt
##' @param moh.event.dt
##' @param moh.op.dt
##' @param asa.dt
##' @param facility.lookup.dt
##' @param ethnicity.lookup.dt
##' @param ineligible.chapters
##' @param ineligible.procedure.strings
generate.revision.one.event.opdate.dt <-
  function(adhb.theatre.event.dt,
           adhb.event.dt,
           moh.event.dt,
           moh.op.dt,
           asa.dt,
           facility.lookup.dt,
           ethnicity.lookup.dt,
           ineligible.chapters,
           ineligible.procedure.strings,
           pre.period.start,
           pre.period.end,
           post.period.start,
           post.period.end) {
    

    
    # Attach facility name and DHB (not strictly necessary, could do it by code)
    moh.event.dt = merge(x = moh.event.dt,
                         y = facility.lookup.dt[,.(FACILITY = `Health Facility Code`, 
                                                   Facility.name = Name,
                                                   DHB = `DHB Name`,
                                                   Facility.type = `Facility Type Name`)],
                         by = 'FACILITY',
                         all.x = TRUE)
    
    # Attach priority ethnicity
    moh.event.dt = merge(
      x = moh.event.dt,
      y = unique(ethnicity.lookup.dt[, .(code.L2,
                                         ethnicg.desc.L1 = desc.L1,
                                         ethnicg.desc.L2 = desc.L2)]), 
      by.x = 'ETHNICGP',
      by.y = 'code.L2',
      all.x = TRUE
    )
    moh.event.dt[ETHNICGP == 54, ethnicg.desc.L1 := 'Other']
    moh.event.dt[ETHNICGP == 54, ethnicg.desc.L2 := 'Other']
    
    # Create rows for every unique event and operation date combination
    revision.one.event.opdate.dt = merge(x = moh.event.dt,
                                         y = unique(moh.op.dt[, .(EVENT_ID = as.character(EVENT_ID), OP_ACDTE)]),
                                         by = 'EVENT_ID',
                                         all.x = TRUE)
    
    
    adhb.event.data = merge(adhb.theatre.event.dt,
                            adhb.event.dt,
                            by = 'Event ID',
                            all.x = TRUE)
    # adhb.event.data = merge(adhb.event.data,
    #                         adhb.revision.one.recalculated.patient.dt,
    #                         by = 'Event ID',
    #                         all.x = TRUE)
    
    
    
    adhb.event.data = adhb.event.data[,.SD[`Actual Into Theatre Date Time` == min(`Actual Into Theatre Date Time`)][1], by = 'Event ID']
    
    #Match index events provided by ACH to MOH moh.op.dt. 
    #Assigns the indexEventID to the corresponding event in eventData.
    #Returns a table of unmatched index events
    
    #Match on the basis of patient and operation date.
    indexOpData = merge(adhb.event.data[,.(NHI,`Event ID`,`Theatre Event ID`,`Actual Into Theatre Date`)], 
                        revision.one.event.opdate.dt, 
                        by.x = c("NHI", "Actual Into Theatre Date"), 
                        by.y = c("PRIM_HCU", "OP_ACDTE"), 
                        allow.cartesian = FALSE,
                        all.x = TRUE)
    
    
    indexOpData = unique(indexOpData, by = "Theatre Event ID")
    
    setkey(indexOpData,"Theatre Event ID")
    
    
    
    #Do a bit of fuzzy matching, either one day in front or behind, make sure it's not outside the bounds.
    daysToMatch = c(-1, 1)
    
    for (opDateMod in daysToMatch) {
      
      newMatches = merge(
        indexOpData[is.na(EVENT_ID),
                    .(NHI,
                      `Event ID`,
                      `Theatre Event ID`,
                      `Actual Into Theatre Date` = `Actual Into Theatre Date` + opDateMod)],
        revision.one.event.opdate.dt[((OP_ACDTE >= pre.period.start) &
                                        (OP_ACDTE <= pre.period.end) |
                                        (OP_ACDTE >= post.period.start) &
                                        (OP_ACDTE <= post.period.end)
        )],
        by.x = c("NHI", "Actual Into Theatre Date"),
        by.y = c("PRIM_HCU", "OP_ACDTE"),
        allow.cartesian = FALSE
      )
      
      indexOpData = rbindlist(list(
        indexOpData[!`Theatre Event ID` %in% newMatches[,`Theatre Event ID`]],
        newMatches
      ))
    }
    
    revision.one.event.opdate.dt = indexOpData
    setnames(revision.one.event.opdate.dt, old = 'Actual Into Theatre Date', new = 'OP_ACDTE')
    
    
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
    
    revision.one.event.opdate.dt = merge(x = revision.one.event.opdate.dt,
                                         y = unique.asa.dt,
                                         by.x = c('EVENT_ID', 'OP_ACDTE'),
                                         by.y = c('moh.event.id', 'moh.op.date'),
                                         all.x = TRUE
    )
    
    
    # Attach acuity
    moh.event.dt[ADM_TYPE == 'AC', acuity := 'Acute']
    moh.event.dt[ADM_TYPE != 'AC', acuity := 'Not acute']
    
    moh.event.dt[, acuity := factor(
      acuity,
      levels = c('Not acute', 'Acute'),
      labels = c('Not acute', 'Acute')
    )]
    
    revision.one.event.opdate.dt = merge(
      x = revision.one.event.opdate.dt,
      y = moh.event.dt[,.(EVENT_ID, acuity)],
      by = c('EVENT_ID'),
      all.x = TRUE
    )
    
    # # Attach primary operation from MOH, plus description.
    # # Exclude ineligible procedures, then get the first per EVENT_ID/OPDATE combo
    # if ((length(ineligible.chapters)>0) | (length(ineligible.procedure.strings)>0)) {
    #   ineligible.procedure.strings.regex = paste(ineligible.procedure.strings, collapse = '|')
    # 
    #   eligible.eventid.opdate.dt = moh.op.dt[!((desc.chapter %in% ineligible.chapters) |
    #                                              (
    #                                                grepl(
    #                                                  ineligible.procedure.strings.regex,
    #                                                  desc.procedure,
    #                                                  ignore.case = TRUE
    #                                                )
    #                                              ))]
    # } else {
    #   eligible.eventid.opdate.dt = moh.op.dt
    # }
    eligible.eventid.opdate.dt = moh.op.dt
    
    first.eligible.eventid.opdate.dt = eligible.eventid.opdate.dt[, .SD[DIAG_SEQ == min(DIAG_SEQ)], by = .(EVENT_ID, OP_ACDTE)]
    
    revision.one.event.opdate.dt = merge(
      x = revision.one.event.opdate.dt,
      y = first.eligible.eventid.opdate.dt[, .(
        EVENT_ID = as.character(EVENT_ID),
        OP_ACDTE,
        code.chapter,
        desc.chapter,
        code.procedure = CLIN_CD,
        desc.procedure
      )],
      # by.x = c("EVENT_ID", "Actual Into Theatre Date"),
      by = c("EVENT_ID", "OP_ACDTE"),
      all.x = TRUE
    )
    
  
  
  return(revision.one.event.opdate.dt)

}
