#' Check new optimum for uniqueness
#' @description
#' Function that checks if a new optimum at \code{argument} is not yet contained
#' in \code{L}.
#' @param L
#' A list of identified optima which contains lists with
#' \itemize{
#'   \item \code{value} and
#'   \item \code{argument}
#' }
#' of each identified optimum.
#' @param argument
#' The argument of a candidate optimum.
#' @param tolerance
#' A non-negative numeric value. For an identified optimum and a candidate
#' optimum, if all coordinate differences are smaller than `tolerance`, they
#' are considered as equal.
#' \code{tolerance},
#' @return
#' A boolean. If \code{TRUE}, \code{argument} is not contained in \code{L}.
#' If \code{FALSE}, \code{argument} is already contained in \code{L}.

unique_optimum = function(L, argument, tolerance){
  for(i in seq_len(length(L)))
    if(isTRUE(all.equal(target = L[[i]]$argument,
                        current = argument,
                        tolerance = tolerance)))
      return(FALSE)
  return(TRUE)
}
