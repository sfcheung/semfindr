#' @title Standardized Case Influence on Parameter Estimates (Approximate)
#'
#' @description Gets a [lavaan::lavaan()] output and computes the
#' approximate standardized changes in selected parameters for each case
#' if included.
#'
#' @details For each case, [est_change_approx()] computes the
#' approximate differences in the estimates of selected parameters
#' with and without this case:
#'
#' (Estimate with all case) - (Estimate without this case)
#'
#' The differences are standardized by dividing
#' the approximate raw differences by their standard errors. This is a
#' measure of the standardized influence of a case on the parameter estimates
#' if it is included.
#'
#' If the value of a case is positive, including the case increases an estimate.
#'
#' If the value of a case is negative, including the case decreases an estimate.
#'
#' The model is not refitted. Therefore, the result is only an
#' approximation of that of [est_change()]. However, this
#' approximation is useful for identifying potentially influential
#' cases when the sample size is very large or the model takes a long
#' time to fit. This function can be used to identify potentially
#' influential cases quickly and then select them to conduct the
#' leave-one-out sensitivity analysis using [lavaan_rerun()] and
#' [est_change()].
#'
#' This function also computes the approximate generalized Cook's
#' distance (gCD). To avoid confusion, it is labelled `gcd_approx`.
#'
#' For the technical details, please refer to the vignette
#' on this approach: \code{vignette("casewise_scores", package = "semfindr")}
#'
#' The approximate approach does not yet support a model with
#' equality constraints.
#'
#' Currently it only supports single-group models.
#'
#' @param fit The output from [lavaan::lavaan()] or its wrappers (e.g.,
#' [lavaan::cfa()] and [lavaan::sem()]).
#'
#' @param parameters A character vector to specify the selected
#' parameters. Each parameter is named as in `lavaan` syntax, e.g.,
#' `x ~ y` or `x ~~ y`, as appeared in the columns `lhs`, `op`, and `rhs`
#' in the output of [lavaan::parameterEstimates()].
#' Supports specifying an operator to select all parameters with these
#' operators: `~`, `~~`, `=~`, and `~1`. This vector can contain
#' both parameter names and operators. More details can be found
#' in the help of [pars_id()].
#' If omitted or `NULL`, the
#' default, changes on all free parameters will be computed.
#'
#' @param case_id If it is a character vector of length equals to the
#' number of cases (the number of rows in the data in `fit`), then it
#' is the vector of case identification values. If it is `NULL`, the
#' default, then `case.idx` used by `lavaan` functions will be used
#' as case identification values.
#'
#' @param allow_inadmissible If `TRUE`, accepts a fit object with
#' inadmissible results (i.e., `post.check` from
#' [lavaan::lavInspect()] is `FALSE`). Default is `FALSE`.
#'
#' @param skip_all_checks If `TRUE`, skips all checks and allows
#' users to run this function on any object of the `lavaan` class.
#' For users to experiment this and other functions on models
#' not officially supported. Default is `FALSE`.
#'
#' @return An `est_change`-class object, which is
#' matrix with the number of columns equals to the number of
#' requested parameters plus one, the last column being the
#' approximate generalized Cook's
#' distance. The number of rows equal to the number
#' of cases. The row names are the case identification values used in
#' [lavaan_rerun()]. The elements are approximate standardized
#' differences.
#' A print method is available for user-friendly output.
#'
#' @author Idea by Mark Hok Chio Lai <https://orcid.org/0000-0002-9196-7406>,
#' implemented by Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.
#'
#'
#' @examples
#' library(lavaan)
#'
#' # A path model
#'
#' dat <- pa_dat
#' mod <-
#' "
#' m1 ~ a1 * iv1 + a2 * iv2
#' dv ~ b * m1
#' a1b := a1 * b
#' a2b := a2 * b
#' "
#' # Fit the model
#' fit <- lavaan::sem(mod, dat)
#' summary(fit)
#'
#' # Approximate standardized changes and gCD
#' out_approx <- est_change_approx(fit)
#' head(out_approx)
#'
#' # Fit the model several times. Each time with one case removed.
#' # For illustration, do this only for the first 10 cases.
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
#'                           to_rerun = 1:10)
#' # Compute the changes in chisq if a case is removed
#' out <- est_change(fit_rerun)
#' head(out)
#'
#' # Compare the results
#' plot(out_approx[1:10, 1], out[, 1])
#' abline(a = 0, b = 1)
#' plot(out_approx[1:10, 2], out[, 2])
#' abline(a = 0, b = 1)
#' plot(out_approx[1:10, 3], out[, 3])
#' abline(a = 0, b = 1)
#' plot(out_approx[1:10, "gcd_approx"], out[, "gcd"])
#' abline(a = 0, b = 1)
#'
#' # A CFA model
#'
#' dat <- cfa_dat
#' mod <-
#' "
#' f1 =~  x1 + x2 + x3
#' f2 =~  x4 + x5 + x6
#' f1 ~~ f2
#' "
#' # Fit the model
#' fit <- lavaan::cfa(mod, dat)
#' summary(fit)
#'
#' # Approximate standardized changes and gCD
#' # Compute gCD only for free loadings
#' out_approx <- est_change_approx(fit,
#'                                 parameters = "=~")
#' head(out_approx)
#'
#' # A latent variable model
#'
#' dat <- sem_dat
#' mod <-
#' "
#' f1 =~  x1 + x2 + x3
#' f2 =~  x4 + x5 + x6
#' f3 =~  x7 + x8 + x9
#' f2 ~   a * f1
#' f3 ~   b * f2
#' ab := a * b
#' "
#' # Fit the model
#' fit <- lavaan::sem(mod, dat)
#' summary(fit)
#'
#' # Approximate standardized changes and gCD
#' # Compute gCD only for structural paths
#' out_approx <- est_change_approx(fit,
#'                                 parameters = "~")
#' head(out_approx)
#'
#'
#'
#' @export
#' @importMethodsFrom lavaan vcov

est_change_approx <- function(fit,
                       parameters = NULL,
                       case_id = NULL,
                       allow_inadmissible = FALSE,
                       skip_all_checks = FALSE
                       ) {
  if (missing(fit)) {
      stop("No lavaan output supplied.")
    }
  if (!inherits(fit, "lavaan")) {
      stop("The fit object is not a lavaan output.")
    }

  if (!skip_all_checks) {
    check_out <- approx_check(fit, print_messages = FALSE)

    if (check_out != 0) {
        if ((check_out == -1) &&
            !(suppressWarnings(lavaan::lavInspect(fit, "post.check"))) &&
            allow_inadmissible) {
          } else {
            stop(attr(check_out, "info"))
          }
      }
    }

  n <- lavaan::lavTech(fit, "nobs")
  if (is.null(case_id)) {
      # Assume the model is a single-group model
      case_ids <- lavaan::lavInspect(fit, "case.idx")
    } else {
      if (length(case_id) != n) {
          stop("The length of case_id is not equal to the number of cases.")
        } else {
          case_ids <- case_id
        }
    }
  est0 <- lavaan::parameterTable(fit)
  # Do not use user labels
  est0$label <- ""
  est0$lavlabel <- lavaan::lav_partable_labels(est0,
                                               type = "user")
  parameters_names <- est0[est0$free > 0, "lavlabel"]
  if (!is.null(parameters)) {
      parameters_selected <- pars_id(parameters,
                                    fit = fit,
                                    where = "coef")
    } else {
      parameters_selected <- seq_len(length(parameters_names))
    }
  param_idx <- parameters_selected
  x0 <- est_change_raw_approx(fit = fit,
                              parameters = parameters, case_id = case_id)
  s0 <- lavaan::lavScores(fit)[, param_idx, drop = FALSE]
  v0 <- lavaan::vcov(fit)[param_idx, param_idx, drop = FALSE]
  v1 <- diag(1 / sqrt(diag(v0)))
  info0 <- lavaan::lavInspect(fit, what = "information")[param_idx, param_idx,
                                                         drop = FALSE]
  out0 <- x0 %*% v1 * n / (n - 1)
  colnames(out0) <- parameters_names[parameters_selected]
  gcd_approx <- rowSums(
      (x0 %*% info0 * (n - 1)) * x0
    )
  out <- cbind(out0, gcd_approx)
  colnames(out) <- c(parameters_names[parameters_selected], "gcd_approx")
  rownames(out) <- case_ids

  attr(out, "call") <- match.call()
  attr(out, "change_type") <- "standardized"
  attr(out, "method") <- "approx"
  attr(out, "standardized") <- FALSE

  class(out) <- c("est_change", class(out))

  out
}
