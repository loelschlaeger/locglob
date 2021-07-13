### load code
source("load_code.R")

### Gramacy & Lee (2012) function
### 1 global minimum
### minimal function value: -2.873899
f = function(x) {
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
npar = 1

### visualize
x = seq(-1,2.5,0.01)
y = sapply(x,function(x)f(x)$value)
ggplot(data.frame(x = x, y = y), aes(x = x, y = y))+
  geom_line()

### run locglob
controls = list(minimize = TRUE,
                nmax = 5,
                init_runs = 1000,
                init_min = -1,
                init_max = 1,
                init_iterlim_short = 20,
                init_iterlim_long = 1000,
                iterlim = 100,
                interrupt_rate = 0.5,
                neighbor_number = 10,
                neighbor_beta = 0.05)
out = search(f, npar, controls)
