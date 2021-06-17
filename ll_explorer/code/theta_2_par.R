### transformation of theta to par
### theta: list of estimated model parameters

theta_2_par = function(theta) {
  
  ### computes matrix dimension from number of lower-triangular elements (lte)
  dim = function(lte){
    out = -0.5 + sqrt(0.5^2 + 2*lte)
    stopifnot(out%%1==0)
    return(out)
  }
  
  b = theta$b
  
  O = matrix(0,dim(length(theta$o)),dim(length(theta$o)))
  O[lower.tri(O,diag=TRUE)] = theta$o
  
  L = matrix(0,dim(length(theta$l)+1),dim(length(theta$l)+1))
  L[lower.tri(L,diag=TRUE)] = c(1,theta$l)
  
  par = list(b = b, O = O, L = L)
  
  return(par)
}