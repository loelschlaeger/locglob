### simulate parameter of MMNP model
### J: number of choice alternatives
### P: number of choice characteristics

sim_par = function(J,P,seed=1){
  
  set.seed(1)
  
  ### input checks
  stopifnot(c(J,P)%%1==0)
  stopifnot(c(J,P)>0)
  stopifnot(J>1)
  
  b = round(runif(P,-5,5),1)
  Omega = round(rWishart(1,P,diag(P))[,,1],1)
  while(TRUE){
    Sigma = rWishart(1,J,diag(J))[,,1]
    Sigma = Sigma/Sigma[1,1]
    Sigma = round(Sigma,1)
    if(class(try(chol(Sigma),silent=TRUE))[1] != "try-error") break
  }
  
  return(list(b = b, 
              O = t(chol(Omega)), 
              L = t(chol(Sigma)), 
              Omega = Omega, 
              Sigma = Sigma))
}