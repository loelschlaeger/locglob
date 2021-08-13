#' Interrupt local search.
#' @description
#' Function that checks if the local search can be interrupted.
#' @inheritParams search
#' @param point
#' The current location of the local search.
#' @inheritParams unique
#' @return
#' Invisibly boolean, whether to interrupt or not.

interruption = function(f, point, L, minimize) {

  ### no interruption if 'L' is empty
  if(length(L)==0) return(FALSE)

  ### the value at the best iterate in L
  f_best = do.call(what = ifelse(minimize,min,max),
                   args = list(unlist(lapply(L,function(x) x$value))))

  for(i in seq_len(length(L))){
    ### compute distance between 'point' and identified optima
    dist = norm(matrix(point - L[[i]]$argument),"F")

    ### check if distance is below 1
    if(dist <= 1){
      cat(" [optimum already visted]")
      return(TRUE)
    }
  }

  ### check for convergence to a local optimum
  if(1e-3 >= norm(matrix(f(point)$gradient),"F") &&
     3 >= f(point)$value - f_best) {
    cat(" [optimum seems local]")
    return(TRUE)
  }

  ### otherwise do not interrupt
  return(FALSE)
}
