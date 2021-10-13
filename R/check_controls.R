#' Check \code{controls}.
#' @description
#' This function checks the input \code{controls} for the vntrs package.
#' @param controls
#' Either \code{NULL} or a named list with the following elements. Missing
#' elements are set to the default values in parentheses.
#' \itemize{
#'   \item \code{init_runs} (\code{5}):
#'   The number of initial searches.
#'   \item \code{init_min} (\code{-1}):
#'   The minimum argument value for the random initialization.
#'   \item \code{init_max} (\code{1}):
#'   The maximum argument value for the random initialization.
#'   \item \code{init_iterlim} (\code{20}):
#'   The number of iterations for the initial searches.
#'   \item \code{neighborhoods} (\code{5}):
#'   The number of nested neighborhoods.
#'   \item \code{neighbors} (\code{5}):
#'   The number of neighbors in each neighborhood.
#'   \item \code{beta} (\code{0.05}):
#'   A non-negative weight factor to account for the function's curvature in the
#'   selection of the neighbors. If \code{beta = 0}, the curvature is ignored.
#'   The higher the value, the higher the probability of selecting a neighbor in
#'   the direction of the highest function curvature.
#'   \item \code{iterlim} (\code{1000}):
#'   The maximum number of iterations to be performed before the local search is
#'   terminated.
#'   \item \code{tolerance} (\code{1e-6}):
#'   A positive scalar giving the tolerance for comparing different optimal
#'   arguments for equality.
#'   \item \code{time_limit} (\code{NULL}):
#'   The time limit in seconds for the algorithm.
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
  if(is.null(controls[["init_iterlim"]]))
    controls[["init_iterlim"]] = 20
  if(!is.numeric(controls[["init_iterlim"]]))
    stop("'controls$init_iterlim' must be a numeric.")
  if(is.null(controls[["neighborhoods"]]))
    controls[["neighborhoods"]] = 5
  if(!(is.numeric(controls[["neighborhoods"]]) &&
       controls[["neighborhoods"]]>0 && controls[["neighborhoods"]]%%1==0))
    stop("'controls$neighborhoods' must be a positive number.")
  if(is.null(controls[["neighbors"]]))
    controls[["neighbors"]] = 5
  if(!(is.numeric(controls[["neighbors"]]) &&
       controls[["neighbors"]]>0 && controls[["neighbors"]]%%1==0))
    stop("'controls$neighbors' must be a positive number.")
  if(is.null(controls[["beta"]]))
    controls[["beta"]] = 0.05
  if(!(is.numeric(controls[["beta"]]) && controls[["beta"]] >= 0))
    stop("'controls$beta' must be greater zero.")
  if(is.null(controls[["iterlim"]]))
    controls[["iterlim"]] = 1000
  if(!is.numeric(controls[["iterlim"]]))
    stop("'controls$iterlim' must be a numeric.")
  if(is.null(controls[["tolerance"]]))
    controls[["tolerance"]] = 1e-6
  if(!(is.numeric(controls[["tolerance"]]) && controls[["tolerance"]] >= 0))
    stop("'controls$tolerance' must be non-negative.")
  if(!is.null(controls[["time_limit"]]))
    if(!is.numeric(controls[["time_limit"]]))
      stop("'controls$time_limit' must be a numeric.")
  return(controls)
}
