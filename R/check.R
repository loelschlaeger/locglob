#' Check controls for the variable neighborhood trust region search.
#' @description Function that checks the controls for the variable neighborhood trust region search.
#' @details See the README file for details.
#' @param controls A list of controls.
#' @return \code{controls}

check = function(controls){

  stopifnot(controls$interrupt_rate >= 0 & controls$interrupt_rate <= 1)

  return(controls)
}
