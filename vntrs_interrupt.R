### target:  objective function to optimize
### par_curr: current parameter vector
### L:       set of found local optima

vntrs_interrupt = function(target, par_curr, L) {
  
  ### the value at the best iterate in L
  f_best = max(unlist(lapply(L,function(x) target(x)$value)))
  
  ### check if the algorithm seems to converge to an already identified local optimum
  if(any( unlist((lapply(L,function(x) norm(matrix(par_curr - x),"F")))) <= 1 )) return(TRUE)
  
  ### check if the gradient norm is not too small when the value of the objective function is far away from f_best
  if(norm(matrix(target(par_curr)$gradient),"F") <= 1e-3 & f_best - target(par_curr)$value >= 3) return(TRUE)
  
  ### do not interrupt
  return(FALSE)
}