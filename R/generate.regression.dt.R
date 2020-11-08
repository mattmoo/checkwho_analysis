##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param daoh.dt
##' @param moh.patient.dt 
##' @param adhb.patient.dt Needed as a workaround, as MOH did not provide gender.
##' @param event.opdate.dt 
generate.regression.dt <- function(daoh.dt, 
                                   moh.patient.dt,
                                   adhb.patient.dt,
                                   event.opdate.dt) {

  col.names = c(
    "index.event.id",
    "PRIM_HCU",
    "moh.event.id",
    "adhb.event.id",
    "moh.op.date",
    "time.series.eligible.and.unique",
    "pre.eligible.and.unique",
    "post.eligible.and.unique",
    "daoh.period.start",
    "daoh",
    "mort.30.day",
    "mort.90.day"
  )
  
  col.names = col.names[col.names %in% names(daoh.dt)]
  
  # regression.dt = daoh.dt[,
  #                         .(
  #                           index.event.id,
  #                           PRIM_HCU,
  #                           moh.event.id,
  #                           adhb.event.id,
  #                           # adhb.theatre.event.id,
  #                           moh.op.date,
  #                           time.series.eligible.and.unique,
  #                           pre.eligible.and.unique,
  #                           post.eligible.and.unique,
  #                           daoh.period.start,
  #                           daoh,
  #                           mort.30.day,
  #                           mort.90.day
  #                           # ,
  #                           # exceeds.daoh.10,
  #                           # exceeds.daoh.25,
  #                           # exceeds.daoh.median,
  #                           # exceeds.daoh.75
  #                         )]
  
  regression.dt = daoh.dt[,
                          col.names,
                          with = FALSE
                          ]
  
  # Calculate and group age
  regression.dt = merge(x = regression.dt,
                        y = moh.patient.dt[,
                                           .(PRIM_HCU,
                                             date_of_birth)],
                        by = 'PRIM_HCU',
                        all.x = TRUE)
  
  regression.dt[,
                age := interval(date_of_birth, daoh.period.start)/years(1)]
  
  regression.dt[,
                age.group := cut(x = age,
                                 breaks = c(0,16,34,49,65,79,130),
                                 labels = c("0-15",
                                            "16-33",
                                            "34-48",
                                            "49-64",
                                            "65-78",
                                            "79+"), 
                                 right = FALSE
                )]
  
  
  #Attach predictors
  regression.dt = merge(
    x = regression.dt,
    y = event.opdate.dt[, .(EVENT_ID,
                            OP_ACDTE,
                            ethnicg.desc.L1,
                            asa.status = factor(asa.status, ordered = FALSE),
                            clinical.severity = max.block.clinical.severity,
                            asa.acuity,
                            gender = factor(GENDER, levels = c('M','F'), labels = c('Male', 'Female')),
                            icd.chapter = desc.chapter,
                            acuity,
                            CCI)],
    by.x = c('moh.event.id','moh.op.date'),
    by.y = c('EVENT_ID','OP_ACDTE'),
    sort = FALSE,
    all.x = TRUE
  )
  
  
  # Group rarer chapters.
  other.icd.chapters = unique(c(regression.dt[pre.eligible.and.unique==TRUE,.N,by = icd.chapter][N<0.05*sum(N)][,icd.chapter],
                                regression.dt[post.eligible.and.unique==TRUE,.N,by = icd.chapter][N<0.05*sum(N)][,icd.chapter]))
  regression.dt[,
                icd.chapter.grouped := icd.chapter]
  regression.dt[icd.chapter %in% other.icd.chapters,
                icd.chapter.grouped := 'Other']
  regression.dt[,
                icd.chapter.grouped := factor(icd.chapter.grouped,
                                              levels = regression.dt[,.N,by = icd.chapter.grouped][order(-N)][,icd.chapter.grouped])]
  
  # Group ASA 4 and 5
  regression.dt[asa.status %in% c('ASA 4', 'ASA 5'), asa.status := 'ASA 4-5']
  regression.dt[, asa.status := factor(asa.status, ordered = FALSE)]
  
  # Remove order from clinical severity
  regression.dt[, clinical.severity := factor(clinical.severity, ordered = FALSE)]
  
  # Group ethnicity
  regression.dt[, ethnicity := ethnicg.desc.L1]
  regression.dt[ethnicg.desc.L1 %in% c(
    "Middle Eastern/Latin American/African (MELAA)",
    "Residual categories",
    "Other ethnicity"
  ),
  ethnicity := 'Other']
  regression.dt[, ethnicity := factor(
    x = ethnicity,
    levels = c("Maori", "European", "Asian", "Pacific Peoples", "Other"),
    labels = c("Maori", "European", "Asian", "Pacific Peoples", "Other")
  )]
  
  # Maori ethnicity was a registered endpoint
  regression.dt[, maori.ethnicity := factor(ethnicity == 'Maori')]
  
  # # Attach gender
  # regression.dt = merge(x = regression.dt,
  #                       y = adhb.patient.dt[, .(PRIM_HCU = NHI, gender = factor(`Gender Desc`))],
  #                       by = 'PRIM_HCU',
  #                       all.x = TRUE)
  
  # Convert DAOH to numeric
  regression.dt[, daoh := as.numeric(daoh)]
  
  return(regression.dt)
}
