{
  ### load dependencies
  installed_packages = installed.packages()[,"Package"]
  required_packages = c()
  for(package in required_packages)
    if(!package %in% installed_packages)
      suppressMessages(install.packages(package))
  rm("installed_packages","required_packages","package")

  ### load .R files
  r_files = list.files(path="R/",pattern=".R")
  for(file in r_files) source(paste0("R/",file))
  rm("r_files","file")

  ### load .cpp files
  cpp_files = list.files(path="src/",pattern=".cpp")
  cpp_files = cpp_files[-which(cpp_files %in% c("RcppExports.cpp",
                                                "RcppExports.o"))]
  for(file in cpp_files) Rcpp::sourceCpp(paste0("src/",file))
  rm("cpp_files","file")

  message("locglob installed.")
}
