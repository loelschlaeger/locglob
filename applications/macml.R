### clear environment and load code
rm(list = ls())
try(setwd("../Discrete_Choice_Procs/RMACML"),silent=TRUE)
source("Rprobit/load_code.R")
options(digits=10)

### define mod_macml object
mod_macml = setup_macml(form = choice ~ V1 | 0, 
                        mod = list(N=100, alt=4, Tp=10),
                        re = c("V1"),
                        corr_re = FALSE,
                        seed = 1,
                        control = list(approx_method="SJ",hess=1))
data_tr = substract_choice_regressor_from_data(mod_macml$data)

### define negative log-likelihood function
target = function(x){
  ll = ll_macml(theta = x, data = data_tr, mod = mod_macml$mod, control = mod_macml$control)
  value = ll
  attr(value,"gradient") = NULL
  attr(value,"hessian") = NULL
  attr(value,"hessian1") = NULL
  attr(value,"grad_lp") = NULL
  return(list(value = value, gradient = attr(ll,"gradient"), hessian = attr(ll,"hessian")))
}

### number of parameter
npar = length(mod_macml$theta_0)

### controls
vntrs_controls = list(minimize = TRUE,
                      nmax = 2,
                      init_runs = 5,
                      init_min = -1,
                      init_max = 1,
                      init_iterlim_short = 20,
                      init_iterlim_long = 100,
                      iterlim = 100,
                      interrupt_rate = 0,
                      neighbor_number = 5,
                      neighbor_beta = 0.05)

### vntrs
start = Sys.time()
out = vntrs(target, npar, vntrs_controls)
end = Sys.time()

difftime(end,start,"mins")