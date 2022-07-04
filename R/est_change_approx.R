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
#' m1 ~ iv1 + iv2
#' dv ~ m1
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
#' plot(out_approx[, "gcd_approx"], out[, "gcd"]); abline(a = 0, b = 1)
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
  parameters_names <- paste0(est0$lhs, est0$op, est0$rhs)
  parameters_names <- parameters_names[est0$free > 0]
  if (!is.null(parameters)) {
    parameters_selected <- est_names_selected(est0, parameters)
    if (!all(parameters_selected %in% parameters_names)) {
        stop(paste("Not all parameters can be found in the output.",
                  "Please check the parameters argument."))
      }
    } else {
      parameters_selected <- parameters_names
    }
  x0 <- est_change_raw_approx(fit = fit, parameters = parameters, case_id = case_id)
  s0 <- lavaan::lavScores(fit)[, parameters_selected, drop = FALSE]
  v0 <- lavaan::vcov(fit)[parameters_selected, parameters_selected, drop = FALSE]
  v1 <- diag(1 / sqrt(diag(v0)))
  info0 <- lavaan::lavInspect(fit, what = "information")[parameters_selected, parameters_selected, drop = FALSE]
  out0 <- x0 %*% v1 * n / (n - 1)
  # gcd_approx <- rowSums((x0 * n) * (x0 %*% v0 / n))
  gcd_approx <- rowSums((s0 %*% v0 %*% info0 * n) * x0)
  out <- cbind(out0, gcd_approx)
  colnames(out) <- c(parameters_selected, "gcd_approx")
  rownames(out) <- case_ids
  out
}
