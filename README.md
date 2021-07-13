# README of locglob

## controls

### general controls
- minimize: A boolean, specifying whether to minimize (TRUE) or to maximize (FALSE).
- nmax: The maximal number of nested neighborhoods.

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
- neighbor_number: The number of neighbors in each neighborhood.
- neighbor_beta: A weight factor to account for the function's curvature in the neighborhood selection.
