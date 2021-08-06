#' Identify local and global optima
#' @description
#' Function that identifies local and global optima via variable neighborhood
#' trust region search.
#' @details
#' You can tune the algorithm by specifying \code{controls}, see the README file
#' for details.
#' @references
#' Bierlaire et al. (2009) "A Heuristic for Nonlinear Global Optimization"
#' <https://doi.org/10.1287/ijoc.1090.0343>
#' @param f
#' A function that computes value, gradient, and Hessian of the function to be
#' optimized and returns them as a named list with elements \code{value},
#' \code{gradient}, and \code{hessian}.
#' @param npar
#' The number of parameters of \code{f}.
#' @param minimize
#' A boolean, determining whether \code{f} should be minimized
#' \code{minimize = TRUE} (the default) or maximized \code{minimize = FALSE}.
#' @inheritParams check_controls
#' @return
#' A list of
#' \itemize{
#'   \item \code{global} optima and
#'   \item \code{local} optima.
#' }
#' @examples
#' ### Shubert function, 18 global minima, 742 local minima
#' ### minimal function value: -186.7309
#' shubert = function(x) {
#'  stopifnot(is.numeric(x))
#'  stopifnot(length(x) == 2)
#'  f = expression(
#'    (1*cos(2*x1+1) + 2*cos(3*x1+2) + 3*cos(4*x1+3) + 4*cos(5*x1+4) +
#'    5*cos(6*x1+5)) *
#'    (1*cos(2*x2+1) + 2*cos(3*x2+2) + 3*cos(4*x2+3) + 4*cos(5*x2+4) +
#'    5*cos(6*x2+5))
#'  )
#'  g1 = D(f, "x1")
#'  g2 = D(f, "x2")
#'  h11 = D(g1, "x1")
#'  h12 = D(g1, "x2")
#'  h22 = D(g2, "x2")
#'  x1 = x[1]
#'  x2 = x[2]
#'  f = eval(f)
#'  g = c(eval(g1), eval(g2))
#'  h = rbind(c(eval(h11), eval(h12)), c(eval(h12), eval(h22)))
#'  list(value = f, gradient = g, hessian = h)
#' }
#' controls = list()
#' search(f = shubert, npar = 2, controls = controls)
#' @export

search = function(f, npar, minimize = TRUE, controls = NULL) {

  ### check input
  stopifnot(is.function(f))
  stopifnot(is.numeric(npar), npar>0, npar%%1==0)
  stopifnot(is.logical(minimize))
  controls = check(controls)

  ### initialization of variable neighborhood search
  initialization = initialize(f = f, npar = npar, controls = controls)
  L = initialization$L
  x_best = initialization$x_best

  ### iterative variable neighborhood search
  k = 1
  while(k <= controls$nmax){

    ### select neighbors
    z = neighbors(f = f, x = x_best, neighborhood_size = k, controls = controls)

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
