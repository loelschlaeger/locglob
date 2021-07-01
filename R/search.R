#' Perform variable neighborhood trust region search.
#' @description Function that performs variable neighborhood trust region search.
#' @details See the README file for details and examples on how to specify \code{controls}.
#' @references This is an implementation of the heuristic presented in "A Heuristic for Nonlinear Global Optimization" (Bierlaire et al., 2009) <https://doi.org/10.1287/ijoc.1090.0343>.
#' @param target A function that computes value, gradient, and Hessian of the function to be optimized and returns them as a list with components \code{value}, \code{gradient}, and \code{hessian}.
#' @param npar The number of parameters of \code{target}.
#' @param controls A list of controls.
#' @return The point at which the optimal value of \code{target} is obtained.
#' @export

search = function(target, npar, controls) {

  ### check controls
  controls = check_controls(controls)

  ### initialization of variable neighborhood search
  initialization = initialize(target = target, npar = npar, controls = controls)
  L = initialization$L
  x_best = initialization$x_best

  ### iterative variable neighborhood search
  k = 1
  while(k <= controls$nmax){

    ### select neighbors
    z = neighborhood(target = target, x = x_best, neighborhood_size = k, controls = controls)

    ### perform local search around neighbors
    local_searches = list()
    for(j in 1:length(z)){
      local_searches[[j]] = local_search(target = target,
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
