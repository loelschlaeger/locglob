#' Print method for \code{locglob}.
#' @param x
#' The output of \code{\link{search}}, which is an object of class
#' \code{locglob}.
#' @param ...
#' ignored
#' @export

print.vntrs = function(x,...) {
  cat("Optimal function value:",x$global[[1]]$value,"\n")
  cat("Number of global optima:",length(x$global),"\n")
  cat("Number of local optima:",length(x$local),"\n")
  return(invisible(NULL))
}
