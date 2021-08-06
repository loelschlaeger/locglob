### create grid with two flexible parameters and log-likelihood values
### x: range of values for flexible parameter number 1
### y: range of values for flexible parameter number 2
### par: list of all model parameters
### flex: list of
### - par: two-dimensional vector, elements either "b", "l" or "o"
### - index: two-dimensional vector, elements indices of the flexible parameter
### data: output from sim_MMNP

make_grid = function(x,y,par,flex,data) {
  
  ### input checks
  stopifnot(is.list(flex))
  stopifnot(length(flex)==2)
  theta = par_2_theta(par)
  for(i in 1:2){
    if(any(is.na(theta[[flex$par[i]]][flex$index[i]]),is.null(theta[[flex$par[i]]][flex$index[i]]))){
      stop(paste0("Parameter ",flex$par[i],flex$index[i]," does not exist."))
    }
  }
    
  grid = expand.grid(x=x,y=y)
  ll = numeric(length(grid))
  
  ### calculate log-likelihood at grid points
  for(row in 1:nrow(grid)){
    
    ### report progress
    cat(sprintf("%.0f%% \r",(row-1)/nrow(grid)*100,appendLF=FALSE))
    
    theta = par_2_theta(par)
    
    for(i in 1:2) theta[[flex$par[i]]][flex$index[i]] = grid[row,i]
    
    ll[row] = comp_ll(theta,data)
    
  }
  cat("Done.")
  
  return(list(values=data.frame(grid,z=ll),par=par,flex=flex))
}
