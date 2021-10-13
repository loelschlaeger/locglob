test_that("Gramacy & Lee function example works", {
  gramacy_lee = function(x) {
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
  expect_snapshot(vntrs(f = gramacy_lee, npar = 1, seed = 1))
})

test_that("Shubert function example works", {
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
  expect_snapshot(vntrs(f = shubert, npar = 2, seed = 1))
})

test_that("Rosenbrock function example works", {
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
  expect_snapshot(vntrs(f = rosenbrock, npar = 2, seed = 1))
})


