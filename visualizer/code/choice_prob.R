### Calculation of choice probability for alternative i
### i: index of chosen alternative
### X: matrix of attributes
### par: a list of the following model parameters
### - b: mean vector of mixing distribution of length P
### - O: lower Cholesky root of covariance matrix of mixing distributions of dimension PxP 
### - L: lower Cholesky root of error-term covariance matrix of dimension (J-1) x (J-1) 

choice_prob = function (i,X,par) {
  
  ### extract model parameters
  b = par$b
  O = par$O
  L = par$L
  J = nrow(L)
  
  ### compute the argument of the MVN CDF as the differences in systmatic utilities
  arg = -delta(J,i) %*% X %*% b
  
  ### compute the matrix of covariances for mixing distributions
  Omega = O %*% t(O)
  
  ### compute the matrix of covariances for errors
  Sigma = L %*% t(L)
  
  ### compute the matrix Gamma 
  Gamma = delta(J,i) %*% ( X %*% Omega %*% t(X) + Sigma ) %*% t(delta(J,i))
  
  # calculate the probability of choosing alternative i
  prob =  mvtnorm::pmvnorm(lower=rep(-Inf,J-1),upper=as.vector(arg),mean=rep(0,J-1),sigma=Gamma)[1]
  
  return(prob)
}