# vntrs <img src="man/figures/logo.png" align="right" height=136 />

<!-- badges: start -->
[![R-CMD-check](https://github.com/loelschlaeger/vntrs/workflows/R-CMD-check/badge.svg)](https://github.com/loelschlaeger/vntrs/actions)
[![CRAN status](https://www.r-pkg.org/badges/version-last-release/vntrs)](https://www.r-pkg.org/badges/version-last-release/vntrs)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/vntrs)](https://cranlogs.r-pkg.org/badges/grand-total/vntrs)
<!-- badges: end -->

## How to get started?

1. Install the latest version of **vntrs** via running `install.packages("vntrs")` in your R console.

2. Specify an R function `f` that computes value, gradient, and Hessian of the function to be optimized and returns them as a named list with elements `value`, `gradient`, and `hessian`.

3. Call `vntrs::search(f = f, npar = npar, minimize = minimize)`, where

  - `npar` is the number of parameters of `f` and
  
  - `minimize` is a boolean, determining whether `f` should be minimized (`minimize = TRUE`, the default) or maximized (`minimize = FALSE`).
  
Optionally, you can tune the algorithm by specifying the named list `controls` and passing it to `search`, see the help file of `search` for details.

## Global and local optima

There is one important element in `controls`, called `only_global`: Set `only_global = TRUE` if you are only interested in the global optimum of `f`. In this case, the algorithm will interrupt any local search prematurely that seems to converge to a local optimum. This saves computation time. If `only_global = FALSE` (the default), the algorithm also looks for local optima. 
