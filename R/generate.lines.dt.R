##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param gaussian.linear.daoh.intervention.changepoint.mcp.fit
##' @param n.lines
##' @param prior
generate.lines.dt <- function(mcp.fit,
                              n.lines, 
                              prior) {

  
  facet_by = NULL
  lines = n.lines
  geom_data = "point"
  cp_dens = TRUE
  q_fit = FALSE
  q_predict = FALSE
  rate = TRUE
  prior = FALSE
  which_y = "mu"
  arma = TRUE
  nsamples = 2000
  scale = "response"
  
  
  # Just for consistent naming in mcp
  fit = mcp.fit
  
  # Useful vars
  xvar = rlang::sym(fit$pars$x)
  yvar = rlang::sym(fit$pars$y)
  is_arma = length(fit$pars$arma) > 0
  
  
  ############################
  # MAKE NEWDATA AND PREDICT #
  ############################
  newdata = tibble::tibble(!!xvar := mcp:::get_eval_at(fit, facet_by))
  
  
  # Predict
  local_pp_eval = function(type) {
    mcp:::pp_eval(
      object = fit,
      newdata = newdata,
      summary = FALSE,  # Get samples
      type = type,
      rate = rate,
      prior = prior,
      which_y = which_y,
      varying = FALSE,
      arma = arma,
      nsamples = nsamples,
      samples_format = "tidy",
      scale = scale
    ) %>%
      dplyr::rename(!!yvar := !!type)  # from "predict"/"fitted" to yvar (response name)
  }
  
  
  lines.dt = as.data.table(local_pp_eval("fitted"))
  lines.dt = lines.dt[.draw %in% sample(unique(.draw), size = min(length(unique(.draw)), n.lines))]
  
  # Give them a super unique ID
  lines.dt[, iteration.id := paste0(.chain,'.',.iteration,'.',.draw)]
  
  # Mark line segments if you want to style them.
  lines.dt[, line.segment := 1]
  if ('cp_1' %in% names(lines.dt) & 'cp_2' %in% names(lines.dt)) {
    lines.dt[date.numeric >= cp_1 &
               date.numeric <= cp_2, line.segment := 2]
    lines.dt[date.numeric > cp_2,
             line.segment := 3]
  }
  
  return(lines.dt)
  

}
