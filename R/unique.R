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
#' \code{L}
#' @example
#' unique(L = list(list(value = 1, argument = c(0,0))), argument = c(0,0))

unique = function(L, argument){
  for(i in seq_len(length(L)))
    if(all(L[[i]]$argument==argument))
      return(FALSE)
  return(TRUE)
}
