evaluate = function(L,controls) {

  if(length(L) == 0) return(NULL)

  ### remove success information
  for(i in 1:length(L)) L[[i]]$success = NULL

  ### remove redundant optima
  local = list(L[[1]])
  for(i in 2:length(L)){
    if(!list(round(L[[i]]$argument,5)) %in% lapply(local,function(x)round(x$argument,5))){
      local = c(local, list(L[[i]]))
    }
  }

  ### return global and local optima
  if(controls$minimize){
    ind_gl = which(unlist(lapply(local,function(x)x$value)) == min(unlist(lapply(local,function(x)x$value))))
  } else {
    ind_gl = which(unlist(lapply(local,function(x)x$value)) == max(unlist(lapply(local,function(x)x$value))))
  }
  global = local[ind_gl]
  local[ind_gl] = NULL
  out = list(global, local)
  names(out) = paste(c("global","local"),ifelse(controls$minimize,"min","max"),sep="_")

  return(out)

}
