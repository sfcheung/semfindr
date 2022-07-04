#' @title Case Influence on Parameter Estimates (Approximated)
#'
#' @description Gets a [lavaan::lavaan()] output and estimates the
#'  changes in selected parameters for each case.
#'
#' @details For each case, [est_change_raw_approx()] computes the
#'  approximated differences
#'  in the estimates of selected parameters with and without this
#'  case: (estimate with all case) - (estimate without this case). The
#'  change is the raw change, either for the standardized or
#'  unstandardized solution. The change is *not* divided by standard
#'  error.
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
#' Currently it only supports single-group models.
#'
#' @param rerun_out The output from [lavaan_rerun()].
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
#' @return A matrix with the number of columns equals to the number of
#'  requested parameters, and the number of rows equals to the number
#'  of cases. The row names are case identification values. The
#'  elements are the raw differences.
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
#' # Compute the approximated changes in parameter estimates if a case is removed
#' out_approx <- est_change_raw_approx(fit)
#' head(out_approx)
#' # Fit the model n times. Each time with one case removed.
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE)
#' # Compute the changes in parameter estimates if a case is removed
#' out <- est_change_raw(fit_rerun)
#' # Results excluding a case, for the first few cases
#' head(out)
#' # Compare the results
#' plot(out_approx[, 1], out[, 1]); abline(a = 0, b = 1)
#' plot(out_approx[, 5], out[, 5]); abline(a = 0, b = 1)
#'
#' @author Idea by Mark Hok Chio Lai <https://orcid.org/0000-0002-9196-7406>,
#'         Implemented by Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.
#'
#'
#' @export
#' @importMethodsFrom lavaan vcov

est_change_raw_approx <- function(fit,
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
  out0 <- lavaan::lavScores(fit) %*% vcov(fit) *
              n / (n - 1)
  out <- out0[, parameters_selected, drop = FALSE]
  rownames(out) <- case_ids
  out
}
