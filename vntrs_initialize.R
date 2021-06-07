#' Initialize variable neighborhood trust region search.
#' @description Function that initializes the variable neighborhood trust region search.
#' @param target A function that computes value, gradient, and Hessian of the function to be optimized and returns them as a list with components \code{value}, \code{gradient}, and \code{hessian}.
#' @param npar The number of parameters of \code{target}.
#' @param vntrs_controls A list of controls.
#' @return A list of the set of identified local optima and best initial parameter values.

vntrs_initialize = function(target, npar, vntrs_controls){
  
  ### initialize list of identified local optima
  L = list()
  
  ### random starting points (row-wise)
  y = runif(vntrs_controls$init_runs*npar, vntrs_controls$init_min, vntrs_controls$init_max)
  y = matrix(y, nrow = vntrs_controls$init_runs, ncol = npar)
  
  ### search locally (for a small number of iterations) at random starting points
  local_searches = list()
  for(n in 1:vntrs_controls$init_runs){
    
    ### perform local search
    local_search_short = trust::trust(objfun = target, 
                                      parinit = y[n,], 
                                      rinit = 1,
                                      rmax = 10, 
                                      iterlim = vntrs_controls$init_iterlim_short, 
                                      minimize = vntrs_controls$minimize)
    
    ### save local search 
    local_searches[[n]] = list("success" = local_search_short$converged, 
                               "value" = local_search_short$value,
                               "argument" = local_search_short$argument)
    
    ### save local optimum (if one has been found)
    if(local_searches[[n]]$success){
      L = c(L,list(local_searches[[n]]$argument))
    }
  }
  
  ### select best candidate
  candidates = unlist(lapply(local_searches,function(x) target(x$argument)$value))
  if(vntrs_controls$minimize){
    j_hat = which.min(candidates)
  } else {
    j_hat = which.max(candidates)
  }
  
  ### check if best candidate is local optimum
  if(local_searches[[j_hat]]$success){
    ### save best candidate
    x_best = local_searches[[j_hat]]$argument
    
  } else {
    ### search locally again longer if no local optimum has been found yet
    local_search_long = trust::trust(objfun = target, 
                                     parinit = local_searches[[j_hat]]$argument, 
                                     rinit = 1,
                                     rmax = 10, 
                                     iterlim = vntrs_controls$init_iterlim_long, 
                                     minimize = vntrs_controls$minimize)
    x_best = local_search_long$argument
  }
  
  ### return set of identified local optima and initially best parameter values
  return(list("L" = L, "x_best" = x_best))
}