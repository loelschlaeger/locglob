#' Perform local search based on a trust-region framework.
#' @description Function that performs local search based on a trust-region framework.
#' @details The trust-region framework is provided by \link[trust]{trust}.
#' @param target A function that computes value, gradient, and Hessian of the function to be optimized and returns them as a list with components \code{value}, \code{gradient}, and \code{hessian}.
#' @param par_init
#' @param iterlim A positive integer specifying the maximum number of iterations to be performed before the local search is terminated.
#' @param interrupt_rate A numeric between 0 and 1, determining the rate to check for premature interruption.
#' @param L 
#' @return 

vntrs_local_search = function(target, par_init, iterlim, interrupt_rate, minimize, L) {
  
  ### determine number of batches based on interrupt_rate
  if(length(L) == 0 || interrupt_rate == 0){
    batches = 1
  } else if(interrupt_rate == 1){
    batches = iterlim
  } else {
    batches = min(iterlim, max(interrupt_rate*iterlim,1))
  }
  
  ### perform local search
  for(b in 1:batches){
  
    out = trust::trust(objfun = target, 
                       parinit = par_init, 
                       rinit = 1,
                       rmax = 10, 
                       iterlim = ceiling(iterlim/batches), 
                       minimize = minimize, 
                       blather = FALSE)
    
    ### check if local search can be interrupted prematurely
    if(length(L) > 0) 
      if(vntrs_interrupt(target = target, 
                         par_curr = out$argument, 
                         L = L,
                         minimize = vntrs_controls$minimize)) 
        return(list("success" = FALSE, "value" = NA, "argument" = NA))
    
    par_init = out$argument
  }
  
  return(list("success" = out$converged, "value" = out$value, "argument" = out$argument))
}