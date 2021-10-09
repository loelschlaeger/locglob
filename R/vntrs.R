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
#' ### missing
#' @export

vntrs = function(f, npar, minimize = TRUE, controls = NULL, quiet = TRUE,
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
  out = prepare_output(L = L, minimize = minimize)
  class(out) = "vntrs"

  ### return output
  cat("Done.\n")
  return(out)
}
