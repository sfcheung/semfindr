#' @title Case Influence on Parameter Estimates (Approximate)
#'
#' @description Gets a [lavaan::lavaan()] output and computes the
#' approximate changes in selected parameters for each case
#' if included.
#'
#' @details For each case, [est_change_raw_approx()] computes the
#' approximate differences
#' in the estimates of selected parameters with and without this
#' case:
#'
#' (Estimate with all case) - (Estimate without this case).
#'
#' The change is the approximate raw change. The change is *not* divided by
#' the standard error of an estimate (hence "raw" in the function name).
#' This is a measure of the influence of a case on the parameter
#' estimates if it is included.
#'
#' If the value of a case is positive, including the case increases an estimate.
#'
#' If the value of a case is negative, including the case decreases an estimate.
#'
#' The model is not refitted. Therefore, the result is only an
#' approximation of that of [est_change_raw()]. However, this
#' approximation is useful for identifying potentially influential
#' cases when the sample size is very large or the model takes a long
#' time to fit. This function can be used to identify potentially
#' influential cases quickly and then select them to conduct the
#' leave-one-out sensitivity analysis using [lavaan_rerun()] and
#' [est_change_raw()].
#'
#' Unlike [est_change_raw()], it does not yet support computing the
#' changes for the standardized solution.
#'
#' For the technical details, please refer to the vignette
#' on this approach: \code{vignette("casewise_scores", package = "semfindr")}
#'
#' The approximate approach supports a model with
#' equality constraints (available in 0.1.4.8 and later version).
#'
#' Supports both single-group and multiple-group models.
#' (Support for multiple-group models available in 0.1.4.8 and later version).
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
#' users to run this function on any object of `lavaan` class.
#' For users to experiment this and other functions on models
#' not officially supported. Default is `FALSE`.
#'
#' @return An `est_change`-class object, which is
#' matrix with the number of columns equals to the number of
#' requested parameters, and the number of rows equals to the number
#' of cases. The row names are case identification values. The
#' elements are the raw differences.
#' A print method is available for user-friendly output.
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
#' # Compute the approximate changes in parameter estimates if a case is included
#' # vs. if this case is excluded.
#' # That is, the approximate case influence on parameter estimates.
#' out_approx <- est_change_raw_approx(fit)
#' head(out_approx)
#' # Fit the model several times. Each time with one case removed.
#' # For illustration, do this only for 10 selected cases
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
#'                           to_rerun = 1:10)
#' # Compute the changes in parameter estimates if a case is included
#' # vs. if this case is excluded.
#' # That is, the case influence on the parameter estimates.
#' out <- est_change_raw(fit_rerun)
#' out
#' # Compare the results
#' plot(out_approx[1:10, 1], out[, 1])
#' abline(a = 0, b = 1)
#' plot(out_approx[1:10, 5], out[, 5])
#' abline(a = 0, b = 1)
#'
#' # A CFA model
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
#' # Compute the approximate changes in parameter estimates if a case is included
#' # vs. if this case is excluded.
#' # That is, approximate case influence on parameter estimates.
#' # Compute changes for free loadings only.
#' out_approx <- est_change_raw_approx(fit,
#'                                     parameters = "=~")
#' head(out_approx)
#'
#' # A latent variable model
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
#' # Compute the approximate changes in parameter estimates if a case is included
#' # vs. if this case is excluded.
#' # That is, the approximate case influence on parameter estimates.
#' # Compute changes for structural paths only
#' out_approx <- est_change_raw_approx(fit,
#'                                     parameters = c("~"))
#' head(out_approx)
#'
#'
#' @author Idea by Mark Hok Chio Lai <https://orcid.org/0000-0002-9196-7406>,
#' implemented by Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.
#'
#'
#' @export
#' @importMethodsFrom lavaan vcov

est_change_raw_approx <- function(fit,
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
    check_out <- approx_check(fit, print_messages = FALSE,
                              multiple_group = TRUE,
                              equality_constraint = TRUE)

    if (check_out != 0) {
        if ((check_out == -1) &&
            !(suppressWarnings(lavaan::lavInspect(fit, "post.check"))) &&
            allow_inadmissible) {
          } else {
            stop(attr(check_out, "info"))
          }
      }
    }

  ngroups <- lavaan::lavInspect(fit, "ngroups")
  if (ngroups > 1) {
      n_j <- sapply(lavaan::lavInspect(fit, "data"), nrow)
      n <- sum(n_j)
    } else {
      n <- nrow(lavaan::lavInspect(fit, "data"))
      n_j <- n
    }

  if (is.null(case_id)) {
      case_ids <- lavaan::lavInspect(fit, "case.idx",
                                     drop.list.single.group = FALSE)
      case_ids <- sort(unlist(case_ids, use.names = FALSE))
    } else {
      case_ids <- lavaan::lavInspect(fit, "case.idx",
                                    drop.list.single.group = FALSE)
      if (length(case_id) != n) {
          stop("The length of case_id is not equal to the number of cases.")
        } else {
          case_ids <- case_id
        }
    }
  est0 <- lavaan::parameterTable(fit)
  # Do not use user labels except for user-defined parameters
  # Ensure that plabels are not used as lavlabels
  tmp1 <- est0$plabel[est0$plabel != ""]
  tmp2 <- est0$label %in% tmp1
  tmp3 <- est0$label
  est0$label[tmp2] <- ""
  est0$label[est0$op != ":="] <- ""
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
  vcov0 <- vcov(fit)
  scores0 <- lavaan::lavScores(fit,
                               ignore.constraints = TRUE,
                               remove.duplicated = FALSE)
  out0 <- scores0 %*% vcov0 * n / (n - 1)
  colnames(out0) <- parameters_names
  out <- out0[, parameters_selected, drop = FALSE]
  rownames(out) <- case_ids

  attr(out, "call") <- match.call()
  attr(out, "change_type") <- "raw"
  attr(out, "method") <- "approx"
  attr(out, "standardized") <- FALSE

  class(out) <- c("est_change", class(out))

  out
}

