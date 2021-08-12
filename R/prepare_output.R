#' Prepare output list.
#' @description
#' A function that prepares the output of \code{\link{search}} by distinguishing
#' between local and global optima in `L`.
#' @inheritParams unique
#' @inheritParams search

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
  if(minimize){
    ind_gl = which(unlist(lapply(local,function(x)x$value)) == min(unlist(lapply(local,function(x)x$value))))
  } else {
    ind_gl = which(unlist(lapply(local,function(x)x$value)) == max(unlist(lapply(local,function(x)x$value))))
  }
  global = local[ind_gl]
  local[ind_gl] = NULL
  out = list(global, local)
  names(out) = paste(c("global","local"),ifelse(minimize,"min","max"),sep="_")

  return(out)

}
