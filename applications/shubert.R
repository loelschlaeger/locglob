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

### number of parameter
npar = 2

### controls
vntrs_controls = list(minimize = TRUE,
                      nmax = 5,
                      
                      init_runs = 5,
                      init_min = -10,
                      init_max = 10,
                      init_iterlim_short = 20,
                      init_iterlim_long = 1000,
                      
                      iterlim = 1000,
                      interrupt_rate = 0.5,
                      
                      neighbor_number = 5,
                      neighbor_beta = 0.05)
