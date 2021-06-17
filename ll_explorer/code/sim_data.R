### simulate data from MMNP model
### N: number of decision makers 
### T: number of choice occasions
### J: number of choice alternatives
### P: number of choice characteristics
### par: a list of the following model parameters
### - b: mean vector of mixing distribution of length P
### - O: lower Cholesky root of covariance matrix of mixing distributions of dimension PxP 
### - L: lower Cholesky root of error-term covariance matrix of dimension (J-1) x (J-1) 
### seed: seed for drawing the characteristics and random choice coefficients
### sd: standard deviation for the drawing

sim_data = function (N,T,J,P,par,seed=1,sd=3) {
  
  set.seed(seed)
  
  ### input checks
  stopifnot(c(N,T,J,P)%%1==0)
  stopifnot(c(N,T,J,P)>0)
  stopifnot(J>1)
  
  ### list of data
  data = list()
  
  for (n in 1:N){
    
    ### data of decision maker n
    data[[n]] = list()
    
    ### vector of decisions of length T
    y_n = numeric(T)
    
    # draw from mixing distribution for decision maker n 
    beta_n = par$b + par$O %*% rnorm(P)
    
    for(t in 1:T){
      
      ### matrix of choice characteristics of dimension J x P
      X_nt = matrix(rnorm(J*P,sd=sd),nrow=J,ncol=P)
      
      ### vector of systmatic utilities of length J
      V_nt = X_nt %*% beta_n
      
      ### vector of errors of length J
      e_nt = par$L %*% rnorm(J)
      
      ### vector of utilities for decision maker n of length J
      U_nt = V_nt + e_nt
      
      ### save decision
      y_n[t] = which.max(U_nt)
      
      ### save characteristics
      data[[n]][["X"]][[t]] = X_nt
      
    }
    
    ### save decisions
    data[[n]][["y"]] = y_n
    
  }
  
  return(data)
}