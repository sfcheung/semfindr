#' @title Case Influence on User-Defined Statistics
#'
#' @description Gets a [lavaan_rerun()] output and computes the
#' changes in user-defined statistics for each case if included.
#'
#' @details For each case, [user_change_raw()] computes the differences
#' in user-defined statistics with and without this
#' case:
#'
#' (User statistics with all case) - (User statistics without this case).
#'
#' The
#' change is the raw change. The change is *not* divided by standard
#' error.
#' This is a measure of the influence of a case on the use-defined
#' statistics if it is included.
#'
#' If the value of a case is positive, including
#' the case increases a statistic.
#'
#' If the value of a case is negative, including
#' the case decreases a statistic.
#'
#' The user-defined statistics are computed by a user-supplied
#' function, `user_function`. It must return a
#' vector-like object (which can have only one value).
#' If the vector is not named, names will be created
#' as `user_1`, `user_2`, and so on.
#'
#' @param rerun_out The output from [lavaan_rerun()].
#'
#' @param user_function A function that accepts a
#' `lavaan`-class object. This function is for computing
#' user-defined statistics.
#'
#' @param ... Optional arguments to be
#' passed to `user_function`.
#'
#' @param fit_name If the function does
#' not accept the `lavaan`-class object
#' as its first argument, this should be
#' the name of the argument of
#' `user_function` for the
#' `lavaan`-class object. For example,
#' if the function is of the form `foo(x
#' = "x1", y = "y2", my_fit = fit)`,
#' where `fit` should be the `lavaan`-class
#' object, set `fit_name` to `"my_fit"`
#' for passing teh `lavaan`-class object
#' to the function.
#'
#'
#' @return An `est_change`-class object, which is a
#' matrix with the number of columns equals to the number of
#' values returned by `user_function` when computed in one
#' `lavaan`-class object, and the number of rows equals to
#' the number of cases. The row names are the case
#' identification values used in
#' [lavaan_rerun()]. The elements are the raw differences.
#' A print method is available for user-friendly output.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.
#'
#' @examples
#'
#' # A path model
#'
#' library(lavaan)
#' dat <- pa_dat
#' mod <-
#' "
#' m1 ~ a1 * iv1 + a2 * iv2
#' dv ~ b * m1
#' a1b := a1 * b
#' a2b := a2 * b
#' "
#' # Fit the model
#' fit <- sem(mod, dat)
#' summary(fit)
#' # Fit the model several times. Each time with one case removed.
#' # For illustration, do this only for four selected cases
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
#'                           to_rerun = c(2, 4, 7, 9))
#' # Get the R-squares
#' lavInspect(fit, what = "rsquare")
#' out <- user_change_raw(fit_rerun,
#'                        user_function = lavInspect,
#'                        what = "rsquare")
#' out
#'
#' # Index plot
#' p <- index_plot(out,
#'                 column = "dv",
#'                 plot_title = "R-square: dv")
#' p
#'
#'
#' @references Pek, J., & MacCallum, R. (2011). Sensitivity analysis
#'  in structural equation models: Cases and their influence.
#'  *Multivariate Behavioral Research, 46*(2), 202-228.
#'  doi:10.1080/00273171.2011.561068
#'
#' @export

user_change_raw <- function(rerun_out,
                            user_function = NULL,
                            ...,
                            fit_name = NULL) {
  more_args <- list(...)
  if (missing(rerun_out)) {
      stop("No lavaan_rerun output supplied.")
    }
  if (!is.function(user_function)) {
      stop("'user_function' not a function.")
    }
  case_ids <- names(rerun_out$rerun)
  reruns <- rerun_out$rerun
  fit0   <- rerun_out$fit
  fit_arg <- list(fit0)
  names(fit_arg) <- fit_name
  est <- do.call(user_function,
                 c(fit_arg, more_args))
  if (!is.null(dim(est))) {
      stop("The output of 'user_function' needs to be a vector.")
    }
  # # It now supports unnamed vectors
  # if (is.null(names(est))) {
  #     stop("The output of 'user_function' must be a named vector.")
  #   }
  out0 <- sapply(reruns,
                 function(x, user_function, fit_name, est, more_args) {
                     fit_arg <- list(x)
                     names(fit_arg) <- fit_name
                     est - do.call(user_function,
                                   c(fit_arg, more_args))
                   },
                 user_function = user_function,
                 fit_name = fit_name,
                 est = est,
                 more_args = more_args,
                 simplify = FALSE,
                 USE.NAMES = TRUE)
  out <- do.call(rbind, out0)
  if (is.null(colnames(out))) {
    colnames(out) <- paste0("user_", seq_len(ncol(out)))
  }
  rownames(out) <- case_ids

  attr(out, "call") <- match.call()
  attr(out, "change_type") <- "raw"
  attr(out, "method") <- "leave_one_out"
  attr(out, "standardized") <- NA
  attr(out, "user_function") <- TRUE

  class(out) <- c("est_change", class(out))

  out
}

