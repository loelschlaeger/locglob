#' Select neighbors.
#' @description
#' Function that selects neighbors around a given point \code{x}.
#' @inheritParams vntrs
#' @param x
#' A point in the domain of \code{f}.
#' @param neighborhood_expansion
#' A scaling factor, specifying the expansion of the neighborhood.
#' @return
#' A list points in the domain of \code{f} which neighbors of \code{x}.

select_neighbors = function(f, x, neighborhood_expansion, controls) {

  ### list of neighbors
  z = list()

  ### compute eigenvectors and eigenvalues of the Hessian of 'f'
  H_f = f(x)$hessian
  H_f_eig = eigen(H_f)
  v = H_f_eig$vectors
  lambda = H_f_eig$values

  ### set selection probabilities
  p_v = exp(controls$beta * lambda/neighborhood_expansion)
  p_v = p_v / sum(p_v)

  ### select neighbors
  for(j in 1:controls$neighbors){

    alpha = runif(1,0.75,1)
    dir = sample(c(-1,1),1)

    ### select eigenvector
    v_i = v[,sample(1:length(lambda),size=1,prob=p_v)]

    ### compute neighbor value
    z[[j]] = x + alpha * neighborhood_expansion * dir * v_i
  }

  return(z)
}
