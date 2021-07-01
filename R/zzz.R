.onAttach = function(lib, pkg) {
  # startup message
  msg = c(paste0(
    "locglob version ", packageVersion("locglob")),
    "\nType 'citation(\"locglob\")' for citing this R package in publications.",
    "\nSee https://github.com/loelschlaeger/locglob for references.")
  packageStartupMessage(msg)
  invisible()
}
