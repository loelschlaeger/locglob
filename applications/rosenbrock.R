### Rosenbrock function
target <- function(x) {
  stopifnot(is.numeric(x))
  stopifnot(length(x) == 2)
  f <- expression(100 * (x2 - x1^2)^2 + (1 - x1)^2)
  g1 <- D(f, "x1")
  g2 <- D(f, "x2")
  h11 <- D(g1, "x1")
  h12 <- D(g1, "x2")
  h22 <- D(g2, "x2")
  x1 <- x[1]
  x2 <- x[2]
  f <- eval(f)
  g <- c(eval(g1), eval(g2))
  B <- rbind(c(eval(h11), eval(h12)), c(eval(h12), eval(h22)))
  list(value = f, gradient = g, hessian = B)
}

### valley surface
### 1 global minimum
### minimal function value: 0
library(plotly)
x = seq(-3,3,0.1)
y = seq(-3,3,0.1)
grid = expand.grid(x=x,y=y)
grid$z = apply(grid,1,function(x)target(x)$value)
plot_ly() %>% 
  add_trace(data=grid, x=grid$x, y=grid$y, z=grid$z, type="mesh3d") 

### number of parameter
npar = 2

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

