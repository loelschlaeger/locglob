#' Check controls.
#' @description Function that checks the controls locglob.
#' @details
#' See the README file for details and examples on how to specify \code{controls}.
#' @param controls
#' A list of controls.
#' @return \code{controls}

check = function(controls){

  stopifnot(controls$interrupt_rate >= 0 & controls$interrupt_rate <= 1)

  return(controls)
}
