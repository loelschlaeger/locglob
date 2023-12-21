#' Check function
#'
#' @description
#' This function checks the input \code{f}.
#'
#' @param f
#' A function that computes value, gradient, and Hessian of the function to be
#' optimized and returns them as a named list with elements \code{value},
#' \code{gradient}, and \code{hessian}.
#' @param npar
#' The number of parameters of \code{f}.
#' @inheritParams check_controls
#'
#' @return
#' No return value, called for side-effects.
#'
#' @keywords
#' internal
#'
#' @importFrom stats runif

check_f <- function(f, npar, controls) {
  ### check inputs
  if (!is.function(f)) {
    stop("'f' must be a function.",
      call. = FALSE
    )
  }
  if (!(is.numeric(npar) && npar > 0 && npar %% 1 == 0)) {
    stop("'npar' must be a positive number.",
      call. = FALSE
    )
  }

  ### draw random test points
  test_runs <- 10
  y <- stats::runif(test_runs * npar, controls$init_min, controls$init_max)
  y <- matrix(y, nrow = test_runs, ncol = npar)
  y <- round(y, 1)

  ### perform test
  for (run in seq_len(test_runs)) {
    call <- paste0("f(", paste(y[run, ], collapse = ","), ")")
    out <- try(f(y[run, ]))
    if (inherits(out, "try-error")) {
      stop("Could not compute ", call, ".",
        call. = FALSE
      )
    }
    if (!is.list(out)) {
      stop(call, " does not return a list.",
        call. = FALSE
      )
    }
    if (is.null(out[["value"]])) {
      stop(call, " does not return a list with element 'value'.",
        call. = FALSE
      )
    }
    if (!(is.numeric(out[["value"]]) && length(out[["value"]]) == 1)) {
      stop("The element 'value' in the output list of ", call,
        " is not a single numeric value.",
        call. = FALSE
      )
    }
    if (is.null(out[["gradient"]])) {
      stop(call, " does not return a list with element 'gradient'.",
        call. = FALSE
      )
    }
    if (!(is.numeric(out[["gradient"]]) && length(out[["gradient"]]) == npar)) {
      stop("The element 'gradient' in the output list of ", call,
        " is not a numeric vector of length 'npar'.",
        call. = FALSE
      )
    }
    if (is.null(out[["hessian"]])) {
      stop(call, " does not return a list with element 'hessian'.",
        call. = FALSE
      )
    }
    if (!(is.numeric(out[["hessian"]]) &&
      is.matrix(out[["hessian"]]) &&
      all(dim(out[["gradient"]]) == c(npar, npar)))) {
      stop("The element 'hessian' in the output list of ", call,
        " is not a numeric matrix of dimension 'npar' x 'npar'.",
        call. = FALSE
      )
    }
  }
}
