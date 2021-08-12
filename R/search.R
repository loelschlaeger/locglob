#' Identify local and global optima.
#' @description
#' Function that identifies local and global optima via variable neighborhood
#' trust region search (VNTRS).
#' @references
#' Bierlaire et al. (2009) "A Heuristic for Nonlinear Global Optimization"
#' <https://doi.org/10.1287/ijoc.1090.0343>
#' @inheritParams check_f
#' @param minimize
#' A boolean, determining whether \code{f} should be minimized
#' \code{minimize = TRUE} (the default) or maximized \code{minimize = FALSE}.
#' @inheritParams check_controls
#' @param quite
#' Set `quite = TRUE` to suppress progress printing.
#' @return
#' A list of two list,
#' \itemize{
#'   \item \code{global} optima and
#'   \item \code{local} optima,
#' }
#' each of which contains lists with
#' \itemize{
#'   \item \code{value} and
#'   \item \code{argument}
#' }
#' of each identified optimum.
#' @examples
#' ### Shubert function
#' ### 18 global minima, 742 local minima
#' ### minimal function value: -186.7309
#' shubert = function(x) {
#'  stopifnot(is.numeric(x))
#'  stopifnot(length(x) == 2)
#'  f = expression(
#'    (cos(2*x1+1) + 2*cos(3*x1+2) + 3*cos(4*x1+3) + 4*cos(5*x1+4) +
#'    5*cos(6*x1+5)) *
#'    (cos(2*x2+1) + 2*cos(3*x2+2) + 3*cos(4*x2+3) + 4*cos(5*x2+4) +
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
#' search(f = shubert, npar = 2, minimize = TRUE, controls = controls)
#'
#' ### Gramacy & Lee function
#' ### 1 global minimum
#' ### minimal function value: -2.873899
#' gramacy_lee = function(x) {
#'  stopifnot(is.numeric(x))
#'  stopifnot(length(x) == 1)
#'  f = expression(sin(10*pi*x) / (2*x) + (x-1)^4)
#'  g = D(f, "x")
#'  h = D(g, "x")
#'  f = eval(f)
#'  g = eval(g)
#'  h = eval(h)
#'  list(value = f, gradient = g, hessian = as.matrix(h))
#' }
#' controls = list()
#' search(f = gramacy_lee, npar = 1, minimize = TRUE, controls = controls)
#'
#' ### Rosenbrock function
#' ### 1 global minimum
#' ### minimal function value: 0
#' rosenbrock = function(x) {
#'   stopifnot(is.numeric(x))
#'   stopifnot(length(x) == 2)
#'   f = expression(100 * (x2 - x1^2)^2 + (1 - x1)^2)
#'   g1 = D(f, "x1")
#'   g2 = D(f, "x2")
#'   h11 = D(g1, "x1")
#'   h12 = D(g1, "x2")
#'   h22 = D(g2, "x2")
#'   x1 = x[1]
#'   x2 = x[2]
#'   f = eval(f)
#'   g = c(eval(g1), eval(g2))
#'   h = rbind(c(eval(h11), eval(h12)), c(eval(h12), eval(h22)))
#'   list(value = f, gradient = g, hessian = h)
#' }
#' controls = list()
#' search(f = rosenbrock, npar = 2, minimize = TRUE, controls = controls)
#' @export

search = function(f, npar, minimize = TRUE, controls = NULL, quiet = FALSE) {

  ### check inputs
  if(!(is.numeric(npar) && npar>0 && npar%%1==0))
    stop("'npar' must be a positive number.")
  if(!is.logical(minimize))
    stop("'minimize' must be a boolean.")
  if(!is.logical(quite))
    stop("'quite' must be a boolean.")
  if(quite){
    sink(tempfile())
    on.exit(sink())
  }
  controls = check_controls(controls = controls)
  check_f(f = f, npar = npar, controls = controls)

  ### initialization of variable neighborhood search
  cat("Initialize VNTRS.")
  initialization = initialize(f = f, npar = npar, minimize = minimize,
                              controls = controls)
  L = initialization$L
  x_best = initialization$x_best

  ### iterative variable neighborhood search
  cat("Start VNTRS.")
  k = 1
  while(k <= controls$neighborhoods){

    ### select neighbors
    cat(paste0("* Select neighborhood ",k,".\n"))
    z = select_neighbors(f = f, x = x_best, neighborhood_expansion = 1.5^(k-1),
                         controls = controls)

    ### perform local search around neighbors
    for(j in 1:length(z)){

      ### perform local search
      cat("** Neighbor",j)
      start = Sys.time()
      local_search = local(f = f,
                           parinit = z[[j]],
                           minimize = minimize,
                           controls = controls,
                           L = L)
      end = Sys.time()

      ### save local optimum (if unique one has been found)
      cat(paste0(" [",sprintf("%.0f",difftime(end,start,unit="secs")),"s]"))
      if(local_search$success){
        cat(" [success]")
        if(unique(L = L, argument = local_search$argument)){
          cat(" [unique]")
          L = c(L,list(local_search))
        }
      }
      cat("\n")
    }

    ### if identified better optimum, reset neighborhoods
    f_best = do.call(what = ifelse(minimize,min,max),
                     args = list(unlist(lapply(L,function(x) x$value))))
    if((minimize && f_best < f(x_best)$value) ||
       (!minimize && f_best > f(x_best)$value)){
      cat("* Reset neighborhood, because better optima found.\n")
      k = 1
    } else {
      k = k + 1
    }
  }

  ### prepare and return output
  out = prepare_output(L = L, minimize = minimize)
  class(out) = "locglob"
  return(out)
}
