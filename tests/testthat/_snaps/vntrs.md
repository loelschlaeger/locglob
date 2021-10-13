# Gramacy & Lee function example works

    Code
      vntrs(f = gramacy_lee, npar = 1, seed = 1)
    Output
                p1      value global
      1 -0.3403684  1.8254676  FALSE
      2 -0.5319385  4.7149787  FALSE
      3  0.3478743 -1.2532442  FALSE
      4 -0.1412848 -1.7105432   TRUE
      5  0.5485634 -0.8690111  FALSE

# Shubert function example works

    Code
      vntrs(f = shubert, npar = 2, seed = 1)
    Output
                p1         p2      value global
      1  1.3200043 -0.1953858  -24.93701  FALSE
      2 -0.1953858 -0.8003211 -123.57677   TRUE
      3  0.3342439 -0.1953858  -32.77094  FALSE
      4 -0.8003211  0.8217839  -54.40487  FALSE
      5 -0.8003211 -0.1953858 -123.57677   TRUE
      6 -2.5108773 -0.8003211  -52.05040  FALSE
      7  2.7859345 -0.8003211  -38.29597  FALSE
      8 -4.4775289 -0.8003211  -39.58874  FALSE

# Rosenbrock function example works

    Code
      vntrs(f = rosenbrock, npar = 2, seed = 1)
    Output
        p1        p2        value global
      1  1 0.9999999 2.114623e-15   TRUE

