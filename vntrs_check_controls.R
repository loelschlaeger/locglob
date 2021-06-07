#' Check controls for the variable neighborhood trust region search.
#' @description Function that checks the controls for the variable neighborhood trust region search.
#' @details See the README file for details.
#' @param vntrs_controls A list of controls.
#' @return \code{vntrs_controls}

vntrs_check_controls = function(vntrs_controls){
  
  stopifnot(vntrs_controls$interrupt_rate >= 0 & vntrs_controls$interrupt_rate <= 1)
  
  return(vntrs_controls)
}