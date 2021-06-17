#' Perform variable neighborhood trust region search.
#' @description Function that performs variable neighborhood trust region search.
#' @details See the README file for details and examples on how to specify \code{vntrs_controls}.
#' @references This is an implementation of the heuristic presented in "A Heuristic for Nonlinear Global Optimization" (Bierlaire et al., 2009) <https://doi.org/10.1287/ijoc.1090.0343>. 
#' @param target A function that computes value, gradient, and Hessian of the function to be optimized and returns them as a list with components \code{value}, \code{gradient}, and \code{hessian}.
#' @param npar The number of parameters of \code{target}.
#' @param vntrs_controls A list of controls.
#' @return The point at which the optimal value of \code{target} is obtained.
#' @export

vntrs = function(target, npar, vntrs_controls) {
  
  ### check vntrs_controls
  vntrs_controls = vntrs_check_controls(vntrs_controls = vntrs_controls)
  
  ### initialization of variable neighborhood search
  initialization = vntrs_initialize(target = target, 
                                    npar = npar, 
                                    vntrs_controls = vntrs_controls) 
  L = initialization$L
  x_best = initialization$x_best
  
  ### iterative variable neighborhood search
  k = 1
  while(k <= vntrs_controls$nmax){
    
    ### select neighbors
    z = vntrs_neighborhood(target = target,
                           x = x_best,
                           neighborhood_size = k,
                           vntrs_controls = vntrs_controls)
    
    ### perform local search around neighbors
    local_searches = list()
    for(j in 1:length(z)){
      local_searches[[j]] = vntrs_local_search(target, 
                                               par_init = z[[j]], 
                                               iterlim = vntrs_controls$iterlim, 
                                               interrupt_rate = vntrs_controls$interrupt_rate, 
                                               minimize = vntrs_controls$minimize,
                                               L = L)
      
      ### save local optimum (if one has been found)
      if(local_searches[[j]]$success){
        L = c(L,list(local_searches[[j]]))
      }
    }
    
    if(vntrs_controls$minimize){
      x_best_new = L[[which.min(unlist(lapply(L,function(x) x$value)))]]$argument
    } else {
      x_best_new = L[[which.max(unlist(lapply(L,function(x) x$value)))]]$argument
    }
    
    ### check if better local optimum found
    k = ifelse(any(x_best != x_best_new), 1, k+1)
    x_best = x_best_new
  }
  
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
  if(vntrs_controls$minimize){
    ind_gl = which(unlist(lapply(local,function(x)x$value)) == min(unlist(lapply(local,function(x)x$value))))
  } else {
    ind_gl = which(unlist(lapply(local,function(x)x$value)) == max(unlist(lapply(local,function(x)x$value))))
  }
  global = local[ind_gl]
  local[ind_gl] = NULL
  out = list(global, local)
  names(out) = paste(c("global","local"),ifelse(vntrs_controls$minimize,"min","max"),sep="_")
  return(out)
}