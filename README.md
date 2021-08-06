# README of the R package **locglob**

The purpose of this R package is to find local and global optima of some numerical function via a variable neighborhood trust region search, which makes the optimization fast and efficient. For a reference, see Bierlaire et al. (2009) "A Heuristic for Nonlinear Global Optimization" <https://doi.org/10.1287/ijoc.1090.0343>.

To use **locglob**, follow these steps:

1. Install the latest version of **locglob** via running `install.packages("locglob")` in your R console.

2. Specify an R function `f` that computes value, gradient, and Hessian of the function to be optimized and returns them as a named list with elements `value`, `gradient`, and `hessian`.

3. Call `locglob::search(f = f, npar = npar, minimize = minimize)`, where

  - `npar` is the number of parameters of `f` and
  
  - `minimize` is a boolean, determining whether `f` should be minimized (`minimize = TRUE`, the default) or maximized (`minimize = FALSE`).

Do you found a bug or request a feature? Please [tell us](https://github.com/loelschlaeger/locglob/issues)!

## Specify `controls`

If you which, you can tune the variable neighborhood trust region search by setting the following elements of a named list `controls` and passing it to `search`. Missing elements are set to default values.

- nmax: The maximal number of nested neighborhoods.

- init_runs: A positive integer specifying the number of random initializations.

- init_min: A numeric specifying the minimum value for the random initialization.

- init_max: A numeric specifying the maximum value for the random initialization.

- init_iterlim_short: A positive integer specifying the number of iterations for the short initial searches.

- init_iterlim_long: A positive integer specifying the number of iterations for the long initial search.

- iterlim: A positive integer specifying the maximum number of iterations to be performed before the local search is terminated.

- interrupt_rate: A numeric between 0 and 1, determining the rate to check for premature interruption.

- neighbor_number: The number of neighbors in each neighborhood.

- neighbor_beta: A weight factor to account for the function's curvature in the neighborhood selection.

## Example: The Shubert function

### Gramacy-Lee function

```r
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
```

### Rosenbrock function

```r
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

```

### Shubert function

```r
### Shubert function
target = function(x) {
  stopifnot(is.numeric(x))
  stopifnot(length(x) == 2)
  f = expression( 
    (1*cos(2*x1+1) + 2*cos(3*x1+2) + 3*cos(4*x1+3) + 4*cos(5*x1+4) + 5*cos(6*x1+5)) *
      (1*cos(2*x2+1) + 2*cos(3*x2+2) + 3*cos(4*x2+3) + 4*cos(5*x2+4) + 5*cos(6*x2+5))
  )
  g1 = D(f, "x1")
  g2 = D(f, "x2")
  h11 = D(g1, "x1")
  h12 = D(g1, "x2")
  h22 = D(g2, "x2")
  x1 = x[1]
  x2 = x[2]
  f = eval(f)
  g = c(eval(g1), eval(g2))
  B = rbind(c(eval(h11), eval(h12)), c(eval(h12), eval(h22)))
  list(value = f, gradient = g, hessian = B)
}

### rough surface with many optima
### 18 global minima
### 742 local minima
### minimal function value: -186.7309
library(plotly)
x = seq(-10,10,0.1)
y = seq(-10,10,0.1)
grid = expand.grid(x=x,y=y)
grid$z = apply(grid,1,function(x)target(x)$value)
plot_ly() %>% 
  add_trace(data=grid,  x=grid$x, y=grid$y, z=grid$z, type="mesh3d") 

### number of parameter
npar = 2
```
