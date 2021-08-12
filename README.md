# locglob

👉 The purpose of this R package is to find local and global optima of some numerical function via a variable neighborhood trust region search (VNTRS), which makes the optimization fast and efficient. For a reference, see Bierlaire et al. (2009) "A Heuristic for Nonlinear Global Optimization" <https://doi.org/10.1287/ijoc.1090.0343>.

💬 Do you found a bug or request a feature? Please [tell us](https://github.com/loelschlaeger/locglob/issues)!

📝 In R, type `citation("locglob")` for citing this package in publications.

## How to get started?

1. Install the latest version of **locglob** via running `install.packages("locglob")` in your R console.

2. Specify an R function `f` that computes value, gradient, and Hessian of the function to be optimized and returns them as a named list with elements `value`, `gradient`, and `hessian`.

3. Call `locglob::search(f = f, npar = npar, minimize = minimize)`, where

  - `npar` is the number of parameters of `f` and
  
  - `minimize` is a boolean, determining whether `f` should be minimized (`minimize = TRUE`, the default) or maximized (`minimize = FALSE`).
  
Optionally, you can tune the algorithm by specifying the named list `controls` and passing it to `search`, see the help file of `search` for details.

There is one important element in `controls`, called `only_global`: Set `only_global = TRUE` if you are only interested in the global optimum of `f`. In this case, the algorithm will interrupt any local search prematurely that converges to a local optimum. This saves computation time. If `only_global = FALSE` (the default), the algorithm also looks for local optima. 

## Example 1: The Shubert function

```r
### Shubert function
### 18 global minima, 742 local minima
### minimal function value: -186.7309
shubert = function(x) {
 stopifnot(is.numeric(x))
 stopifnot(length(x) == 2)
 f = expression(
   (cos(2*x1+1) + 2*cos(3*x1+2) + 3*cos(4*x1+3) + 4*cos(5*x1+4) +
   5*cos(6*x1+5)) *
   (cos(2*x2+1) + 2*cos(3*x2+2) + 3*cos(4*x2+3) + 4*cos(5*x2+4) +
   5*cos(6*x2+5))
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
 h = rbind(c(eval(h11), eval(h12)), c(eval(h12), eval(h22)))
 list(value = f, gradient = g, hessian = h)
}
controls = list()
search(f = shubert, npar = 2, minimize = TRUE, controls = controls)
```

## Example 2: Gramacy & Lee function

```r
### Gramacy & Lee function
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
controls = list()
search(f = gramacy_lee, npar = 1, minimize = TRUE, controls = controls)
```

## Example 3: Rosenbrock function

```r
### Rosenbrock function
### 1 global minimum
### minimal function value: 0
rosenbrock = function(x) {
  stopifnot(is.numeric(x))
  stopifnot(length(x) == 2)
  f = expression(100 * (x2 - x1^2)^2 + (1 - x1)^2)
  g1 = D(f, "x1")
  g2 = D(f, "x2")
  h11 = D(g1, "x1")
  h12 = D(g1, "x2")
  h22 = D(g2, "x2")
  x1 = x[1]
  x2 = x[2]
  f = eval(f)
  g = c(eval(g1), eval(g2))
  h = rbind(c(eval(h11), eval(h12)), c(eval(h12), eval(h22)))
  list(value = f, gradient = g, hessian = h)
}
controls = list()
search(f = rosenbrock, npar = 2, minimize = TRUE, controls = controls)
```
