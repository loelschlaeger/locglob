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

## Global and local optima

There is one important element in `controls`, called `only_global`: Set `only_global = TRUE` if you are only interested in the global optimum of `f`. In this case, the algorithm will interrupt any local search prematurely that seems to converge to a local optimum. This saves computation time. If `only_global = FALSE` (the default), the algorithm also looks for local optima. 
