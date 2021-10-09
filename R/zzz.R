.onAttach = function(lib, pkg) {
  # startup message
  msg = paste0(
    "Thanks for installing vntrs ", packageVersion("vntrs"), ", happy search!\n",
    "Type 'citation(\"vntrs\")' for citing the package in publications.")
  packageStartupMessage(msg)
  invisible()
}
