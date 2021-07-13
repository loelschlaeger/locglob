#' Collect identified local optima.
#' @description
#' Function that collects identified local optima.
#' @param L
#' The set of found local optima.
#' @param local_search
#' The result of one local search.
#' @return \code{L}

collect = function(L, local_search){
  for(i in seq_len(length(L))) if(all(L[[i]]$argument==local_search$argument)) return(L)
  return(c(L,list(local_search)))
}
