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
#' @param quiet
#' Set `quiet = FALSE` to print progress.
#' @param seed
#' Set a seed for the sampling of the random starting points.
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
#' ### Gramacy & Lee function
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
#' explore(f = gramacy_lee, npar = 1, seed = 1)
#'
#' ### Shubert function
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
#' explore(f = shubert, npar = 2, seed = 1)
#'
#' ### Rosenbrock function
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
#' explore(f = rosenbrock, npar = 2, seed = 1)
#' @export

explore = function(f, npar, minimize = TRUE, controls = NULL, quiet = TRUE,
                  seed = NULL) {

  ### check inputs
  if(!(is.numeric(npar) && npar>0 && npar%%1==0))
    stop("'npar' must be a positive number.")
  if(!is.logical(minimize))
    stop("'minimize' must be a boolean.")
  if(!is.logical(quiet))
    stop("'quiet' must be a boolean.")
  if(quiet){
    sink(tempfile())
    on.exit(sink())
  }
  if(!is.null(seed))
    set.seed(seed)
  controls = check_controls(controls = controls)
  check_f(f = f, npar = npar, controls = controls)

  ### initialization of variable neighborhood search
  cat("Initialize VNTRS.\n")
  initialization = initialize(f = f, npar = npar, minimize = minimize,
                              controls = controls)
  L = initialization$L
  x_best = initialization$x_best

  ### iterative variable neighborhood search
  cat("Start VNTRS.\n")
  k = 1
  while(k <= controls$neighborhoods){

    ### select neighbors
    cat(paste0("* Select neighborhood ",k,".\n"))
    z = select_neighbors(f = f, x = x_best, neighborhood_expansion = 1.5^(k-1),
                         controls = controls)

    ### perform local search around neighbors
    for(j in seq_len(length(z))){

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
      cat(paste0(" [",sprintf("%.0f",difftime(end,start,units = "auto")),"s]"))
      if(local_search$success){
        cat(" [found optimum]")
        if(unique_optimum(L = L, argument = local_search$argument,
                          tolerance = controls$tolerance)){
          cat(" [optimum is unknown]")
          L = c(L,list(local_search))
        }
      }
      cat("\n")
    }

    ### if identified better optimum, reset neighborhoods
    pos_x_best_new = do.call(what = ifelse(minimize,which.min,which.max),
                             args = list(unlist(lapply(L,function(x) x$value))))
    x_best_new = L[[pos_x_best_new]]$argument
    if(!isTRUE(all.equal(target = x_best_new,
                         current = x_best,
                         tolerance = controls$tolerance))){
      cat("* Reset neighborhood, because better optimum found.\n")
      x_best = x_best_new
      k = 1
    } else {
      k = k + 1
    }
  }

  ### prepare output
  out = prepare_output(L = L, minimize = minimize)
  class(out) = "locglob"

  ### return output
  cat("Done.\n")
  return(out)
}
