### Gramacy & Lee (2012) function
target = function(x) {
  stopifnot(is.numeric(x))
  stopifnot(length(x) == 1)
  f = expression(sin(10*pi*x) / (2*x) + (x-1)^4)
  g = D(f, "x")
  h = D(g, "x")
  f = eval(f)
  g = eval(g)
  h = eval(h)
  list(value = f, gradient = g, hessian = as.matrix(h))
}

### 1 global minimum
### minimal function value: -2.873899
library(ggplot2)
x = seq(-1,2.5,0.01)
y = sapply(x,function(x)target(x)$value)
ggplot(data.frame(x = x, y = y), aes(x = x, y = y))+
  geom_line()

### number of parameter
npar = 1

### controls
vntrs_controls = list(minimize = TRUE,
                      nmax = 5,
                      init_runs = 1000,
                      init_min = -10,
                      init_max = 10,
                      init_iterlim_short = 20,
                      init_iterlim_long = 1000,
                      iterlim = 100,
                      interrupt_rate = 0.5,
                      neighbor_number = 10,
                      neighbor_beta = 0.05)

### vntrs
vntrs(target, npar, vntrs_controls)
