#' @noRd
#' @importFrom utils packageVersion

.onAttach <- function(lib, pkg) {
  msg <- paste0(
    "Thanks for using {vntrs} ", utils::packageVersion("vntrs"),
    ", happy search!\n", "Type 'citation(\"vntrs\")' for citing this package."
  )
  packageStartupMessage(msg)
  invisible()
}
