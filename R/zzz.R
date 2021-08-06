.onAttach = function(lib, pkg) {
  # startup message
  msg = c(
    paste0("Thanks for installing 'locglob' version ",
           packageVersion("locglob"),"."),
    "\nType 'citation(\"locglob\")' for citing the R package in publications.",
    "\nHappy search!")
  packageStartupMessage(msg)
  invisible()
}
