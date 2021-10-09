#' Prepare output list.
#' @description
#' A function that prepares the output of \code{\link{search}} by distinguishing
#' between local and global optima in `L`.
#' @inheritParams unique_optimum
#' @inheritParams vntrs
#' @return
#' A list of two list,
#' \itemize{
#'   \item \code{global} optima and
#'   \item \code{local} optima,
#' }
#' each of which contains lists with
#' \itemize{
#'   \item \code{value} and
#'   \item \code{argument}
#' }
#' of each identified optimum.

prepare_output = function(L, minimize) {

  ### prepare output
  out = list("global" = list(), "local" = list())

  ### no optima identified
  if(length(L) == 0){
    warning("No optima found.")
    return(out)
  }

  ### remove 'success' elements in 'L'
  for(i in seq_len(length(L)))
    L[[i]]$success = NULL

  ### classify local and global optima
  values = unlist(lapply(L,function(x)x$value))
  if(minimize){
    ind_gl = which(values == min(values))
  } else {
    ind_gl = which(values == max(values))
  }
  out[["global"]] = L[ind_gl]
  L[ind_gl] = NULL
  out[["local"]] = L
  return(out)
}
