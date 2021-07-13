#' Identify local and global optima.
#' @description
#' Function that identifies local and global optima applying variable
#' neighborhood trust region search.
#' @details
#' See the README file for details and examples on how to specify \code{controls}.
#' @references
#' Bierlaire et al. (2009) "A Heuristic for Nonlinear Global Optimization"
#' <https://doi.org/10.1287/ijoc.1090.0343>
#' @param f
#' A function that computes value, gradient, and Hessian of the function to be
#' optimized and returns them as a list with components \code{value},
#' \code{gradient}, and \code{hessian}.
#' @param npar
#' The number of parameters of \code{f}.
#' @param controls
#' A list of controls.
#' @return
#' A list of local and global optima of \code{f}.
#' @export

search = function(f, npar, controls) {

  ### check controls
  controls = check(controls)

  ### initialization of variable neighborhood search
  initialization = initialize(f = f, npar = npar, controls = controls)
  L = initialization$L
  x_best = initialization$x_best

  ### iterative variable neighborhood search
  k = 1
  while(k <= controls$nmax){

    ### select neighbors
    z = spread(f = f, x = x_best, neighborhood_size = k, controls = controls)

    ### perform local search around neighbors
    local_searches = list()
    for(j in 1:length(z)){
      local_searches[[j]] = local(f = f,
                                  par_init = z[[j]],
                                  iterlim = controls$iterlim,
                                  interrupt_rate = controls$interrupt_rate,
                                  minimize = controls$minimize,
                                  L = L)

      ### save local optimum (if one has been found)
      if(local_searches[[j]]$success) L = c(L,list(local_searches[[j]]))

    }

    if(controls$minimize){
      x_best_new = L[[which.min(unlist(lapply(L,function(x) x$value)))]]$argument
    } else {
      x_best_new = L[[which.max(unlist(lapply(L,function(x) x$value)))]]$argument
    }

    ### check if better local optimum found
    k = ifelse(any(x_best != x_best_new), 1, k+1)
    x_best = x_best_new

  }

  ### prepare output
  out = evaluate(L,controls)
  return(invisible(out))
}
