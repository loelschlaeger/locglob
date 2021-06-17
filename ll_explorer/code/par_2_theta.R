### transformation of par to theta
### par: list of all model parameters

par_2_theta = function(par) {
  theta = list(b = par$b, o = par$O[lower.tri(par$O,diag=TRUE)], l = par$L[lower.tri(par$L,diag=TRUE)][-1])
  return(theta)
}