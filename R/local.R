#' Perform trust region local search.
#' @description
#' Function that links to \code{\link[trust]{trust}}.
#' @inheritParams check_f
#' @param parinit
#' Passed on to \code{\link[trust]{trust}}.
#' @inheritParams unique
#' @return
#' A list of
#' \itemize{
#'   \item \code{success}: A boolean, determining wether the local search
#'         successfully converged.
#'   \item \code{value}: The value at the point where the local search
#'         terminated.
#'   \item \code{argument}: The point where the local search terminated.
#' }

local = function(f, parinit, minimize, controls, L) {

  ### determine number of batches based on 'controls$interruption_rate'
  if(length(L)==0 || controls$interruption_rate == 0){
    batches = 1
  } else if(controls$interruption_rate == 1){
    batches = controls$iterlim
  } else {
    batches = min(controls$iterlim,
                  max(controls$interruption_rate*controls$iterlim, 1))
  }

  ### perform local search
  for(b in 1:batches){

    out = trust::trust(objfun = f,
                       parinit = parinit,
                       rinit = 1,
                       rmax = 10,
                       iterlim = ceiling(controls$iterlim/batches),
                       minimize = minimize,
                       blather = FALSE)

    ### check if local search can be interrupted prematurely
    if(length(L) > 0)
      if(interruption(f = f, point = out$argument, L = L, minimize = minimize))
        cat(" [interrupted]")
        return(list("success" = FALSE, "value" = NA, "argument" = NA))

    parinit = out$argument
  }

  return(list("success" = out$converged, "value" = out$value,
              "argument" = out$argument))
}
