##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param moh.event.dt
##' @param moh.nnpac.dt
##' @param moh.patient.dt
generate.hospitalisation.dt <- function(moh.event.dt, 
                                        moh.nnpac.dt,
                                        moh.patient.dt) {

  
  hospitalisation.dt = rbindlist(
    list(
      # moh.nnpac.dt[,.(PRIM_HCU, 
      #                 EVSTDATE = date_of_service, 
      #                 EVENDATE = date_of_service)],
      
      moh.event.dt[,.(PRIM_HCU, 
                      EVSTDATE, 
                      EVENDATE)]
    )
  )
  
  # There is a peculiarity where some hospital admissions are after death
  # (particularly from NNPAC) eliminate these.
  hospitalisation.dt = merge(x = hospitalisation.dt,
                             y = moh.patient.dt,
                             by = 'PRIM_HCU',
                             all.x = T)[
                               is.na(date_of_death) | EVSTDATE <= date_of_death][
                                 ,.(PRIM_HCU, date_of_death, EVSTDATE, EVENDATE)
                               ]
  
  return(hospitalisation.dt)
  

}
