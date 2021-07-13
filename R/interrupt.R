#' Interrupt local search.
#' @description
#' Function that interrupts the local search.
#' @param f
#' A function that computes value, gradient, and Hessian of the function to be optimized and returns them as a list with components \code{value}, \code{gradient}, and \code{hessian}.
#' @param par_curr
#' The current parameter vector.
#' @param L
#' The set of found local optima.
#' @return
#' A boolean, whether to interrupt or not.

interrupt = function(f, par_curr, L, minimize) {

  ### the value at the best iterate in L
  if(minimize){
    f_best = min(unlist(lapply(L,function(x) x$value)))
  } else {
    f_best = max(unlist(lapply(L,function(x) x$value)))
  }

  ### check if the algorithm seems to converge to an already identified local optimum
  if(any(unlist((lapply(L,function(x) norm(matrix(par_curr - x$argument),"F")))) <= 1 )) return(TRUE)

  ### check if the gradient norm is not too small when the value of the objective function is far away from f_best
  if(norm(matrix(f(par_curr)$gradient),"F") <= 1e-3 & f_best - f(par_curr)$value >= 3) return(TRUE)

  ### do not interrupt
  return(FALSE)
}
