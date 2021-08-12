#' Interrupt local search.
#' @description
#' Function that checks if the local search can be interrupted.
#' @inheritParams search
#' @param point
#' The current location of the local search.
#' @inheritParams unique
#' @param only_global
#' A boolean. Set \code{only_global = TRUE} if you are only interested in the
#' global optimum. In this case, the algorithm will interrupt any local search
#' prematurely that converges to a local optimum. This saves computation time.
#' If `only_global = FALSE` (the default), the algorithm also looks for local
#' optima.
#' @return
#' A boolean, whether to interrupt or not.

interruption = function(f, point, L, minimize, only_global) {

  ### the value at the best iterate in L
  f_best = do.call(what = ifelse(minimize,min,max),
                   args = list(unlist(lapply(L,function(x) x$value))))

  for(i in seq_len(length(L))){
    ### compute distance between 'point' and identified optima
    dist = norm(matrix(point - L[[i]]$argument),"F")

    ### check if distance is below 1
    if(dist <= 1) return(TRUE)

  }

  ### TODO: add tests

  ### otherwise do not interrupt
  return(FALSE)
}
