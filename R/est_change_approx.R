#' @title Standardized Case Influence on Parameter Estimates (Approximated)
#'
#' @description Gets a [lavaan::lavaan()] output and computes the
#'  standardized changes in selected parameters for each case.
#'
#' @details For each case, [est_change_approx()] computes the
#'  approximated differences in the estimates of selected parameters
#'  with and without this case: (estimate with all case) - (estimate
#'  without this case). The differences are standardized by dividing
#'  the approximated raw differences by their standard errors.
#'
#' The model is not refitted. Therefore, the result is only an
#' approximation of that of [est_change()]. However, this
#' approximation is useful for identifying potentially influential
#' cases when the sample size is very large or the model takes a long
#' time to fit. This function can be used to identify potentially
#' influential cases quickly and then select them to conduct the
#' leave-one-out sensitivity analysis using [lavaan_rerun()] and
#' [est_change_raw()].
#'
#' This function also computes the approximated generalized Cook's
#' distance (gCD). To avoid confusion, it is labelled `gcd_approx`.
#'
#' Currently it only supports single-group models.
#'
#' @param fit The output from [lavaan::lavaan()].
#' @param parameters A character vector to specify the selected
#'  parameters. Each parameter is named as in `lavaan` syntax, e.g.,
#'  `x ~ y` or `x ~~ y`, as appeared in the columns `lhs`, `op`, and `rhs`
#'  in the output of [lavaan::parameterEstimates()].
#'  Supports specifying an operator to select all parameters with this
#'  operators: `~`, `~~`, `=~`, and `~1`. This vector can contain
#'  both parameter names and operators.
#'  If `NULL`, the
#'  default, differences on all free parameters will be computed.
#' @param case_id If it is a character vector of length equals to the
#'  number of cases (the number of rows in the data in `fit`), then it
#'  is the vector of case identification values. If it is `NULL`, the
#'  default, then `case.idx` used by `lavaan` functions will be used
#'  as case identification values.
#'
#' @return A matrix with the number of columns equal to the number of
#'  requested parameters, and the number of rows equal to the number
#'  of cases. The row names are the case identification values used in
#'  [lavaan_rerun()]. The elements are the standardized difference.
#'  Please see Pek and MacCallum (2011), Equation 7.
#'
#' @author Idea by Mark Hok Chio Lai <https://orcid.org/0000-0002-9196-7406>,
#'         Implemented by Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.
#'
#'
#' @examples
#' library(lavaan)
#' dat <- pa_dat
#' # For illustration only, select only the first 50 cases
#' dat <- dat[1:50, ]
#' # The model
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
#' # Approximated standardized changes and gcd
#' out_approx <- est_change_approx(fit)
#' head(out_approx)
#'
#' # Fit the model n times. Each time with one case removed.
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE)
#' # Compute the changes in chisq if a case is removed
#' out <- est_change(fit_rerun)
#' head(out)
#'
#' # Compare the results
#' plot(out_approx[, 1], out[, 1]); abline(a = 0, b = 1)
#' plot(out_approx[, 2], out[, 2]); abline(a = 0, b = 1)
#' plot(out_approx[, 3], out[, 3]); abline(a = 0, b = 1)
#' plot(out_approx[, "gcd_approx"], out[, "gcd"])
#' abline(a = 0, b = 1)
#'
#' # A CFA model
#'
#' dat <- cfa_dat[1:50, ]
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
#' # Approximated standardized changes and gCD
#' # Compute gCD only for free loadings
#' out_approx <- est_change_approx(fit,
#'                                 parameters = "=~")
#' head(out_approx)
#'
#' # A latent variable model
#'
#' dat <- sem_dat[1:50, ]
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
#' # Approximated standardized changes and gCD
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
                       case_id = NULL
                       ) {
  if (missing(fit)) {
      stop("No lavaan output supplied.")
    }
  if (!inherits(fit, "lavaan")) {
      stop("The fit object is not a lavaan output.")
    }
  if (lavaan::lavTech(fit, "ngroups") != 1) {
      stop("Multisample models are not yet supported.")
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
  x0 <- est_change_raw_approx(fit = fit, parameters = parameters, case_id = case_id)
  s0 <- lavaan::lavScores(fit)[, param_idx, drop = FALSE]
  v0 <- lavaan::vcov(fit)[param_idx, param_idx, drop = FALSE]
  v1 <- diag(1 / sqrt(diag(v0)))
  info0 <- lavaan::lavInspect(fit, what = "information")[param_idx, param_idx, drop = FALSE]
  out0 <- x0 %*% v1 * n / (n - 1)
  colnames(out0) <- parameters_names[parameters_selected]
  # gcd_approx <- rowSums((x0 * n) * (x0 %*% v0 / n))
  gcd_approx <- rowSums((s0 %*% v0 %*% info0 * n) * x0)
  out <- cbind(out0, gcd_approx)
  colnames(out) <- c(parameters_names[parameters_selected], "gcd_approx")
  rownames(out) <- case_ids
  out
}
