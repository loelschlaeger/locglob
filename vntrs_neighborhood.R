#' Select neighborhood.
#' @description Function that selects neighbors around a given point.
#' @param target A function that computes value, gradient, and Hessian of the function to be optimized and returns them as a list with components \code{value}, \code{gradient}, and \code{hessian}.
#' @param x A point in the domain of \code{target}.
#' @param neighborhood_size A scaling factor specifying the size of the neighborhood.
#' @param vntrs_controls A list of controls for the variable neighborhood trust region search.
#' @return A list of neighbors.

vntrs_neighborhood = function(target, x, neighborhood_size, vntrs_controls) {
  
  ### list of neighbors
  z = list()
  
  ### compute eigenvectors and -values of Hessian
  H_f = target(x)$hessian
  H_f_eig = eigen(H_f)
  v = H_f_eig$vectors
  lambda = H_f_eig$values
  
  ### tuning parameter
  d_k = 1.5^(k-1)
  
  ### selection probabilities
  p_v = exp(beta*lambda/d_k)
  p_v = p_v / sum(p_v)
  
  ### select neighbors
  for(j in 1:p){
    
    alpha = runif(1,0.75,1)
    dir = sample(c(-1,1),1)
    
    ### select eigenvector
    v_i = v[,sample(1:length(lambda),n=1,prob=p_v)]
    
    ### compute neighbor value
    z[[j]] = x + alpha * d_k * dir * v_i
  }
  
  return(z)
}