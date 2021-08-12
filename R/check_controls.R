#' Check \code{controls}.
#' @description
#' Function that checks \code{controls} for \code{\link{search}}.
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
#'   \item \code{init_iterlim} \code{(20)}:
#'   The number of iterations for the short initial searches.
#'   \item \code{neighborhoods} \code{(5)}:
#'   The number of nested neighborhoods.
#'   \item \code{neighbors} \code{(5)}:
#'   The number of neighbors in each neighborhood.
#'   \item \code{beta} \code{(0.05)}:
#'   A non-negative weight factor to account for the function's curvature in the
#'   selection of the neighbors. If \code{beta = 0}, the curvature is ignored.
#'   The higher the value, the higher probability of selecting a neighbor in the
#'   direction of the highest function curvature.
#'   \item \code{iterlim} \code{(200)}:
#'   The maximum number of iterations to be performed before the local search is
#'   terminated.
#'   \item \code{interruption_rate} \code{(0.1)}:
#'   A numeric between 0 and 1, determining the rate to check if the local
#'   search converges to an already identified optimum and hence can be
#'   interrupted. The higher the value, the more often the algorithm checks
#'   for premature interruption. This in turn increases computation time.
#'   \item \code{only_global} \code{(FALSE)}:
#'   A boolean. Set \code{only_global = TRUE} if you are only interested in the
#'   global optimum. In this case, the algorithm will interrupt any local search
#'   prematurely that converges to a local optimum. This saves computation time.
#'   If `only_global = FALSE` (the default), the algorithm also looks for local
#'   optima.
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
  if(is.null(controls[["n"]]))
    controls[["n"]] = 5
  if(!(is.numeric(controls[["n"]]) && controls[["n"]]>0 &&
       controls[["n"]]%%1==0))
    stop("'controls$n' must be a positive number.")
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
    controls[["iterlim"]] = 200
  if(!is.numeric(controls[["iterlim"]]))
    stop("'controls$iterlim' must be a numeric.")
  if(is.null(controls[["interruption_rate"]]))
    controls[["interruption_rate"]] = 0.1
  if(!(is.numeric(controls[["interruption_rate"]]) &&
       controls[["interruption_rate"]] >= 0 &&
       1 >= controls[["interruption_rate"]]))
    stop("'controls$interruption_rate' must be a number between 0 and 1.")
  if(is.null(controls[["only_global"]]))
    controls[["only_global"]] = FALSE
  if(!is.logical(controls[["only_global"]]))
    stop("'controls$only_global' has to be a boolean.")

  return(controls)
}
