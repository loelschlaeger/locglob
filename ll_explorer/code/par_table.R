### return table of names and values of par elements
### par: a list of the following model parameters
### - b: mean vector of mixing distribution of length P
### - O: lower Cholesky root of covariance matrix of mixing distributions of dimension PxP 
### - L: lower Cholesky root of error-term covariance matrix of dimension (J-1) x (J-1) 

par_table = function(par) {
  theta = par_2_theta(par)
  names = c(paste0("b",1:length(theta$b)),paste0("o",1:length(theta$o)),paste0("l",1:length(theta$l)))
  
  table_all = data.frame("parameter" = c(paste0("b",1:length(theta$b)),paste0("o",1:length(theta$o)),paste0("l",1:length(theta$l))), "true" = round(unlist(theta),2))
  table_b   = data.frame("parameter" = paste0("b",1:length(theta$b)), "true" = round(unlist(theta$b),2))
  table_o   = data.frame("parameter" = paste0("o",1:length(theta$o)), "true" = round(unlist(theta$o),2))
  table_l   = data.frame("parameter" = paste0("l",1:length(theta$l)), "true" = round(unlist(theta$l),2))
  
  return(list(table_all = table_all,
              table_b   = table_b,
              table_o   = table_o,
              table_l   = table_l))
}
