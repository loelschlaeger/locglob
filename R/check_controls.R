#' Check controls.
#' @description
#' Function that checks the controls for \code{\link{search}}.
#' @param controls
#' Either \code{NULL} or a named list with the following elements. Missing
#' elements are set to default values.
#' \itemize{
#'   \item
#' }
#' @return
#' The filled list \code{controls}.

check_controls = function(controls){

  stopifnot(controls$interrupt_rate >= 0 & controls$interrupt_rate <= 1)

  return(controls)
}
