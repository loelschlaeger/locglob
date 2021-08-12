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
#' @return
#' A boolean. If \code{TRUE}, \code{argument} is not contained in \code{L}.
#' If \code{FALSE}, \code{argument} is already contained in \code{L}.

unique = function(L, argument){
  for(i in seq_len(length(L)))
    if(all(L[[i]]$argument==argument))
      return(FALSE)
  return(TRUE)
}
