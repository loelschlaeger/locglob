### calculation of the log-likelihood of a MMNP
### theta: vector of model parameters
### data: list of attributes and choices

comp_ll = function (theta, data) {
  
  par = theta_2_par(theta)
  
  # calculate the log-likelihood
  ll = 0
  
  for(n in 1:length(data)) for(t in 1:length(data[[n]]$X)) 
    ll = ll + log(choice_prob(data[[n]]$y[t],data[[n]]$X[[t]],par))
  
  return(ll)
  
}