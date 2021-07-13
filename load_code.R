{
  installed_packages = installed.packages()[,"Package"]
  exe = alist(
    ### installing and loading packages
    if(!"ggplot2" %in% installed_packages){
      writeLines("\nInstalling package 'ggplot2'.\n")
      install.packages("ggplot2",quite=TRUE)
      },
    require("ggplot2"),
    for(file in grep(list.files(path="src",pattern=".cpp$"),invert=TRUE,pattern="RcppExports.cpp",value=TRUE))
      Rcpp::sourceCpp(paste0("src/",file)),
    for(file in grep(list.files(path="R",pattern=".R$"),invert=TRUE,pattern="RcppExports.R",value=TRUE))
      source(paste0("R/",file))
  )
  for(e in seq_len(length(exe))){
    suppressPackageStartupMessages(eval(exe[[e]]))
    message(sprintf("installing locglob: %.0f%%",(e/length(exe)*100)),"\r",appendLF=FALSE)
  }
  message(sprintf("locglob installed %10s"," "))
  rm(e,exe,file,installed_packages)
}
