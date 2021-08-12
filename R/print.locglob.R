#' Print method for \code{locglob}.
#' @param x
#' The output of \code{\link{search}}, which is an object of class
#' \code{locglob}.
#' @param ...
#' ignored
#' @export

print.locglob = function(x,...) {
  if(!inherits(x,"locglob"))
    stop("'x' is not of class 'locglob'.")
  cat("Number of global optima:",length(x$global),"\n")
  cat("Number of local optima:",length(x$local),"\n")
  return(invisible(NULL))
}
