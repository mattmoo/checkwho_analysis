##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param time.series.figure.dt
##' @param daoh.limits
##' @param moh.event.dt
##' @param moh.patient.dt
##' @param moh.op.dt
generate.date.status.dt <-
  function(time.series.figure.dt,
           daoh.limits,
           moh.event.dt,
           moh.patient.dt,
           moh.op.dt,
           flag.surgery = TRUE) {
    
  # Get all combinations of dates and index events.
  date.dt = expand.grid(
    index.event.id = time.series.figure.dt[, unique(index.event.id)],
    day.number = seq(from = daoh.limits[1], to = daoh.limits[2])
  )
  setDT(date.dt)
  date.dt = merge(x = date.dt,
                  y = time.series.figure.dt[, .(index.event.id,
                                         patient.id = PRIM_HCU,
                                         index.event.date = daoh.period.start)])
  date.dt[, date := index.event.date + day.number]
  
  # Get dt of all hospitalisation days by patient, and cull to those dates of interest.
  hospital.date.dt = moh.event.dt[, .(patient.id = PRIM_HCU, date = seq(EVSTDATE, EVENDATE, by = "day")), by = .(moh.event.id = EVENT_ID)]
  hospital.date.dt = merge(x = date.dt,
                           y = hospital.date.dt,
                           by = c('patient.id', 'date'))
  hospital.date.dt[, status := 'In hospital']
  
  # Get all dead days from the dates of interest
  setkey(moh.patient.dt, PRIM_HCU)
  death.date.dt = date.dt[, .SD[date >= moh.patient.dt[patient.id, date_of_death]], by = patient.id]
  death.date.dt[, status := 'Dead']
  
  # Get all operation days, and cull to those dates of interest.
  op.date.dt = merge(x = unique(moh.op.dt[block.clinical.severity != 0, .(moh.event.id = EVENT_ID, date = OP_ACDTE)]),
                     y = moh.event.dt[, .(moh.event.id = as.numeric(EVENT_ID), patient.id = PRIM_HCU)],
                     all.x = TRUE)
  op.date.dt = merge(x = date.dt,
                     y = op.date.dt,
                     by = c('patient.id', 'date'))
  op.date.dt[, status := 'Surgery']

  
  # All other dates are a DAOH
  date.dt[, status := 'Alive and out of hospital']
  
  
  # Slap them all together.
  date.status.dt = rbindlist(
    list(date.dt,
         hospital.date.dt,
         death.date.dt,
         op.date.dt),
    fill = TRUE,
    use.names = TRUE
  )
  
  if (flag.surgery == TRUE) {
    # Order the statuses to select 
    date.status.dt[, status := factor(
      status,
      levels = c('Alive and out of hospital',
                 'In hospital',
                 'Surgery',
                 'Dead'),
      ordered = TRUE
    )]
  } else {
    date.status.dt[status == 'Surgery', status := 'In hospital']
    date.status.dt[, status := factor(
      status,
      levels = c('Alive and out of hospital',
                 'In hospital',
                 'Dead'),
      ordered = TRUE
    )]
  }
  
  # Get the 'maximum' status for each day and index event, there's your status.
  date.status.dt = date.status.dt[date.status.dt[, .I[which.max(status)],
                                                 by = .(index.event.id, date)]$V1]
  
  # Mark DAOH
  date.status.dt[, daoh := status == 'Alive and out of hospital']
  
  return(date.status.dt)

}
