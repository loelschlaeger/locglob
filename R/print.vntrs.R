#' Print method for an object of class \code{vntrs}.
#' @param x
#' An object of class \code{vntrs}.
#' @param ...
#' Ignored.
#' @return
#' Returns invisibly \code{x}.
#' @export

print.vntrs = function(x,...) {
  cat("Optimum:",x$value[x$global][1],"\n")
  cat("Global optima:",sum(x$global),"\n")
  cat("Local optima:",sum(!x$global),"\n")
  return(invisible(x))
}
