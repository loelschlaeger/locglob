### fun:  objective function to optimize
### max:  if TRUE, maximize. if FALSE, minimize "fun"
### par_curr: current parameter vector
### par_prev: previous parameter vector
### L:       set of found local optima

vntrs_interrupt = function(fun,par_curr,par_prev,L) {
  
  ### the value at the best iterate in L
  f_best = max(unlist(lapply(L,function(x) fun(x)$value)))
  
  ### check if the algorithm seems to converge to an already identified local optimum
  if(any( unlist((lapply(L,function(x) norm(matrix(par_curr - x),"F")))) <= 1 )) return(TRUE)
  
  ### check if the gradient norm is not too small when the value of the objective function is far away from f_best
  if(norm(matrix(fun(par_curr)$gradient),"F") <= 1e-3 & f_best - fun(par_curr) >= 3) return(TRUE)
  
  ### check if a significant reduction in the objective function is achieved
  if(fun(par_curr)$value < fun(par_prev)$value + 0.3 * t(fun(par_prev)$gradient) * fun(par_prev)$hessian )
  
  ### do not interrupt
  return(FALSE)
}