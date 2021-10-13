#' Check new optimum for uniqueness.
#' @description
#' This function checks if a new optimum \code{argument} is not yet contained
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
#' optimum, if all coordinate differences are smaller than \code{tolerance},
#' they are considered as equal.
#' @return
#' A boolean. If \code{TRUE}, \code{argument} is not contained in \code{L}.
#' If \code{FALSE}, \code{argument} is already contained in \code{L}.

unique_optimum = function(L, argument, tolerance){
  for(i in seq_len(length(L)))
    if(sqrt(sum((argument-L[[i]]$argument)^2)) < tolerance)
      return(FALSE)
  return(TRUE)
}
