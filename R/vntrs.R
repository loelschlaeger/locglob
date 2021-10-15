#' Variable neighborhood trust region search.
#' @description
#' This function performs variable neighborhood trust region search.
#' @references
#' Bierlaire et al. (2009) "A Heuristic for Nonlinear Global Optimization"
#' \doi{10.1287/ijoc.1090.0343}.
#' @inheritParams check_f
#' @param minimize
#' If \code{TRUE}, \code{f} gets minimized. If \code{FALSE}, maximized.
#' @inheritParams check_controls
#' @param quiet
#' If \code{TRUE}, progress messages are suppressed.
#' @param seed
#' Set a seed for the sampling of the random starting points.
#' @return
#' A data frame. Each row contains information of an identified optimum. The
#' first \code{npar} columns \code{"p1"},...,\code{"p<npar>"} store the argument
#' values, the next column \code{"value"} has the optimal function values and
#' the last column \code{"global"} contains \code{TRUE} for global optima and
#' \code{FALSE} for local optima.
#' @examples
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
#' vntrs(f = rosenbrock, npar = 2, seed = 1, controls = list(neighborhoods = 1))
#' @export

vntrs = function(f, npar, minimize = TRUE, controls = NULL, quiet = TRUE,
                 seed = NULL) {

  ### check inputs
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
  cat("Check controls.\n")
  controls = check_controls(controls = controls)
  if(!is.null(controls$time_limit))
    start_time = Sys.time()
  cat("Check function.\n")
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
  stop = FALSE
  while(k <= controls$neighborhoods){
    if(stop) break

    ### select neighbors
    cat(paste0("* Select neighborhood ",k,".\n"))
    z = select_neighbors(f = f, x = x_best, neighborhood_expansion = 1.5^(k-1),
                         controls = controls)

    ### perform local search around neighbors
    for(j in seq_len(length(z))){

      ### check total time
      if(!is.null(controls$time_limit)){
        if(difftime(Sys.time(), start_time, units = "secs") > controls$time_limit){
          stop = TRUE
          warning("Stopped early because 'controls$time_limit' reached.",
                  call. = FALSE, immediate. = TRUE, noBreaks. = TRUE)
          break
        }
      }

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
      t = difftime(end,start,units = "auto")
      cat(paste0(" [",sprintf("%.0f",t)," ",units(t),"]"))
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
      cat("* Reset neighborhood, because better optimum was found.\n")
      x_best = x_best_new
      k = 1
    } else {
      k = k + 1
    }
  }

  ### prepare output
  if(length(L) == 0){
    warning("No optima found.")
    return(NULL)
  }
  arguments = sapply(L,function(x)x$argument)
  if(npar > 1)
    arguments = t(arguments)
  values = sapply(L,function(x)x$value)
  global = values == ifelse(minimize,min,max)(values)
  out = data.frame(arguments, values, global)
  colnames(out) = c(paste0("p",1:npar), "value", "global")

  ### return output
  cat("Done.\n")
  return(out)
}
