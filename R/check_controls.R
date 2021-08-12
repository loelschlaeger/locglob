#' Check controls.
#' @description
#' Function that checks the controls for \code{\link{search}}.
#' @param controls
#' Either \code{NULL} or a named list with the following elements. Missing
#' elements are set to default values (printed in parentheses).
#' \itemize{
#'   \item \code{init_runs} \code{(5)}:
#'   The number of short initial searches.
#'   \item \code{init_min} \code{(-1)}:
#'   The minimum value for the random initialization.
#'   \item \code{init_max} \code{(1)}:
#'   The maximum value for the random initialization.
#'   \item \code{init_iterlim_short} \code{(20)}:
#'   The number of iterations of the short initial searches.
#'   \item \code{init_iterlim_long} \code{(200)}:
#'   The number of iterations for the long initial search.
#'   \item \code{n} \code{(5)}:
#'   The number of nested neighborhoods.
#'
#'
#'- iterlim: A positive integer specifying the maximum number of iterations to be performed before the local search is terminated.
#'
#'- interrupt_rate: A numeric between 0 and 1, determining the rate to check for premature interruption.
#'
#'- neighbor_number: The number of neighbors in each neighborhood.
#'
#'- neighbor_beta: A weight factor to account for the function's curvature in the neighborhood selection.
#' }
#' @return
#' The checked and filled list \code{controls}.

check_controls = function(controls){

  if(is.null(controls))
    controls = list()
  if(!is.list(controls))
    stop("'controls' must be a list.")
  if(is.null(controls[["init_runs"]]))
    controls[["init_runs"]] = 5
  if(!(is.numeric(controls[["init_runs"]]) && controls[["init_runs"]]>0 &&
       controls[["init_runs"]]%%1==0))
    stop("'controls$init_runs' must be a positive number.")
  if(is.null(controls[["init_min"]]))
    controls[["init_min"]] = -1
  if(!is.numeric(controls[["init_min"]]))
    stop("'controls$init_min' must be a numeric.")
  if(is.null(controls[["init_max"]]))
    controls[["init_max"]] = 1
  if(!is.numeric(controls[["init_max"]]))
    stop("'controls$init_max' must be a numeric.")
  if(controls[["init_max"]] < controls[["init_min"]])
    stop("'controls$init_max' must not be smaller than controls$init_min.")
  if(is.null(controls[["init_iterlim_short"]]))
    controls[["init_iterlim_short"]] = 20
  if(!is.numeric(controls[["init_iterlim_short"]]))
    stop("'controls$init_iterlim_short' must be a numeric.")
  if(is.null(controls[["init_iterlim_long"]]))
    controls[["init_iterlim_long"]] = 200
  if(!is.numeric(controls[["init_iterlim_long"]]))
    stop("'controls$init_iterlim_long' must be a numeric.")
  if(is.null(controls[["n"]]))
    controls[["n"]] = 5
  if(!(is.numeric(controls[["n"]]) && controls[["n"]]>0 &&
       controls[["n"]]%%1==0))
    stop("'controls$n' must be a positive number.")


  stopifnot(controls$interrupt_rate >= 0 & controls$interrupt_rate <= 1)

  return(controls)
}
