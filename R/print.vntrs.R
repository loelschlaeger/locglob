#' Print method for an object of class \code{vntrs}.
#' @param x
#' An object of class \code{vntrs}.
#' @param ...
#' Ignored
#' @export

print.vntrs = function(x,...) {
  cat("Optimum:",x$global[[1]]$value,"\n")
  cat("Global optima:",length(x$global),"\n")
  cat("Local optima:",length(x$local),"\n")
  return(invisible(NULL))
}
