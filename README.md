# README of vntrs

## vntrs_controls

### general controls
- minimize: A boolean, specifying whether to minimize (TRUE) or to maximize (FALSE).

### initialization
- init_runs: A positive integer specifying the number of random initializations.
- init_min: A numeric specifying the minimum value for the random initialization.
- init_max: A numeric specifying the maximum value for the random initialization
- init_iterlim_short: A positive integer specifying the number of iterations for the short initial searches.
- init_iterlim_long: A positive integer specifying the number of iterations for the long initial search.

### local search
- iterlim: A positive integer specifying the maximum number of iterations to be performed before the local search is terminated.
- interrupt_rate: A numeric between 0 and 1, determining the rate to check for premature interruption.

### neighborhood selection
- neighbor_number: number of neighbors
- neighbor_beta: weight factor