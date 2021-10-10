#' Perform trust region local search.
#' @description
#' Function that links to \code{\link[trust]{trust}}.
#' @inheritParams vntrs
#' @param parinit
#' Passed on to \code{\link[trust]{trust}}.
#' @inheritParams unique_optimum
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

  ### do not check premature interruption if 'L' is empty or
  ### 'controls$only_global' = FALSE
  if(length(L)==0) {
    batches = 1
  } else {
    batches = controls$iterlim
  }

  ### perform local search
  for(b in seq_len(batches)){

    out = trust::trust(objfun = f,
                       parinit = parinit,
                       rinit = 1,
                       rmax = 10,
                       iterlim = controls$iterlim/batches,
                       minimize = minimize,
                       blather = FALSE)

    if(b < batches){
      ### check if already converged
      if(out$converged) break

      ### check if local search can be interrupted prematurely
      if(interruption(f = f, point = out$argument, L = L, minimize = minimize))
          return(list("success" = FALSE, "value" = NA, "argument" = NA))
    }

    parinit = out$argument
  }

  return(list("success" = out$converged, "value" = out$value,
              "argument" = out$argument))
}
