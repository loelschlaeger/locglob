### clear environment, load code and libraries
rm(list = ls())
try(setwd("RMACML"),silent=TRUE)
for(file in list.files(path="LO/ll_visual/code",pattern=".R")) source(paste0("LO/ll_visual/code/",file))
library(ggplot2)

### specify parameters
N = 100
T = 1
J = 3
P = 2
par = sim_par(J,P)

### simulate data
data = sim_data(N,T,J,P,par,seed=2)

### compute log-likelihood values at grid points
grid = make_grid(x=seq(-2,2,0.2),y=seq(-2,2,0.2),par=par,
                 flex=list(par=c("o","o"),index=c(1,2)),data=data)

### contour plot of log-likelihood
make_contour(grid,par)

