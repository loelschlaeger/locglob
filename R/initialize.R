#' Initialize VNTRS.
#' @description
#' Function that initializes the variable neighborhood trust region search.
#' @inheritParams vntrs
#' @return
#' A list of
#' \itemize{
#'   \item the list \code{L} of identified optima which contains lists with
#'     \itemize{
#'       \item \code{value} and
#'       \item \code{argument}
#'     }
#'     of each identified optimum.
#'   \item best initial point \code{x_best}.
#' }

initialize = function(f, npar, minimize, controls){

  ### initialize list of identified optima
  L = list()

  ### random starting points (row-wise)
  y = runif(controls$init_runs*npar, controls$init_min, controls$init_max)
  y = matrix(y, nrow = controls$init_runs, ncol = npar)

  ### search locally (for a small number of iterations) starting from 'y'
  local_searches = list()
  cat("* Apply local search at",controls$init_runs,"random starting points.\n")
  for(n in seq_len(controls$init_runs)){

    ### perform local search
    cat("** Run",n)
    start = Sys.time()
    local_search = trust::trust(objfun = f,
                                parinit = y[n,],
                                rinit = 1,
                                rmax = 10,
                                iterlim = controls$init_iterlim,
                                minimize = minimize)
    end = Sys.time()

    ### save local search
    local_searches[[n]] = list("success" = local_search$converged,
                               "value" = local_search$value,
                               "argument" = local_search$argument)

    ### save local optimum (if unique one has been found)
    t = difftime(end,start,units = "auto")
    cat(paste0(" [",sprintf("%.0f",t)," ",units(t),"]"))
    if(local_searches[[n]]$success){
      cat(" [found optimum]")
      if(unique_optimum(L = L, argument = local_searches[[n]]$argument,
                        tolerance = controls$tolerance)){
        cat(" [optimum is unknown]")
        L = c(L,list(local_searches[[n]]))
      }
    }
    cat("\n")
  }

  ### select best candidate
  candidates = unlist(lapply(local_searches,function(x) x$value))
  j_hat = do.call(what = ifelse(minimize,which.min,which.max),
                  args = list("x" = candidates))

  ### check if best candidate is local optimum
  if(local_searches[[j_hat]]$success){
    ### save initial point
    x_best = local_searches[[j_hat]]$argument

  } else {
    ### search locally again longer if no local optimum has been found yet
    cat(paste0("* Continue the best run ",j_hat,"."))
    local_search_long = trust::trust(objfun = f,
                                     parinit = local_searches[[j_hat]]$argument,
                                     rinit = 1,
                                     rmax = 10,
                                     iterlim = controls$iterlim,
                                     minimize = minimize)
    if(!local_search_long$converged)
      stop("Initialization failed. Consider increasing 'controls$init_runs',
           'controls$init_iterlim' and 'controls$iterlim'.")
    x_best = local_search_long$argument
    cat(" [found optimum]\n")
  }

  ### return set of identified local optima and initially best parameter value
  return(list("L" = L, "x_best" = x_best))
}
