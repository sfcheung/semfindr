#' @title Case Influence on Fit Measures
#'
#' @description Gets a [lavaan_rerun()] output and computes the changes
#' in selected fit measures if a case is included.
#'
#' @details For each case, [fit_measures_change()] computes the
#' differences in selected fit measures with and without this case:
#'
#' (Fit measure with all case) - (Fit measure without this case).
#'
#' If the value of a case is positive, including the case increases an estimate.
#'
#' If the value of a case is negative, including the case decreases an estimate.
#'
#' Note that an increase is an improvement in fit for
#' goodness of fit measures such as CFI and TLI, but a decrease
#' is an improvement in fit for badness of fit measures such as
#' RMSEA and model chi-square.
#' This is a measure of the influence of a case on a fit measure
#' if it is included.
#'
#' If the analysis is not admissible or does not converge when a case
#' is deleted, `NA`s will be turned for the differences of this
#' case.
#'
#' Supports both single-group and multiple-group models.
#' (Support for multiple-group models available in 0.1.4.8 and later version).
#'
#' @param rerun_out The output from [lavaan_rerun()].
#'
#' @param fit_measures The argument `fit.measures` used in
#' [lavaan::fitMeasures]. Default is
#' `c("chisq", "cfi", "rmsea", "tli")`.
#'
#' @param baseline_model The argument `baseline.model` used in
#' [lavaan::fitMeasures]. Default is `NULL`.
#'
#' @return An `fit_measures_change`-class object, which is
#' matrix with the number of columns equals to the number of
#' requested fit measures, and the number of rows equals to the number
#' of cases. The row names are the case identification values used in
#' [lavaan_rerun()].
#' A print method is available for user-friendly output.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.
#'
#' @references Pek, J., & MacCallum, R. (2011). Sensitivity analysis
#' in structural equation models: Cases and their influence.
#' *Multivariate Behavioral Research, 46*(2), 202-228.
#' doi:10.1080/00273171.2011.561068
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
#' # Fit the model n times. Each time with one case removed.
#' # For illustration, do this only for four selected cases
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
#'                           to_rerun = 1:10)
#' # Compute the changes in chisq if a case is included
#' # vs. if this case is removed.
#' # That is, case influence on model chi-squared.
#' out <- fit_measures_change(fit_rerun, fit_measures = "chisq")
#' # Results excluding a case, for the first few cases
#' head(out)
#' # Chi-square will all cases included.
#' (chisq_all <- fitMeasures(fit, c("chisq")))
#' # Chi-square with the first case removed
#' fit_01 <- lavaan::sem(mod, dat[-1, ])
#' (chisq_no_1 <- fitMeasures(fit_01, c("chisq")))
#' # Difference
#' chisq_all - chisq_no_1
#' # Compare to the result from the fit_measures_change
#' out[1, ]
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
#'
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
#'                           to_rerun = 1:10)
#' out <- fit_measures_change(fit_rerun, fit_measures = "chisq")
#' head(out)
#' (chisq_all <- fitMeasures(fit, c("chisq")))
#' fit_01 <- lavaan::sem(mod, dat[-1, ])
#' (chisq_no_1 <- fitMeasures(fit_01, c("chisq")))
#' chisq_all - chisq_no_1
#' out[1, ]
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
#'
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
#'                           to_rerun = 1:10)
#' out <- fit_measures_change(fit_rerun, fit_measures = "chisq")
#' head(out)
#' (chisq_all <- fitMeasures(fit, c("chisq")))
#' fit_01 <- lavaan::sem(mod, dat[-1, ])
#' (chisq_no_1 <- fitMeasures(fit_01, c("chisq")))
#' chisq_all - chisq_no_1
#' out[1, ]
#'
#' @export

fit_measures_change <- function(rerun_out,
                                fit_measures = c("chisq",
                                                 "cfi",
                                                 "rmsea",
                                                 "tli"),
                                baseline_model = NULL) {
  if (missing(rerun_out)) {
      stop("No lavaan_rerun output supplied.")
    }
  case_ids <- names(rerun_out$rerun)
  reruns <- rerun_out$rerun
  fit0   <- rerun_out$fit
  fitm0  <- lavaan::fitMeasures(fit0, fit.measures = fit_measures,
                                      baseline.model = baseline_model)
  out <- sapply(reruns, function(x, fitm0) {
                      chk <- suppressWarnings(lavaan::lavTech(x, "post.check"))
                      chk2 <- lavaan::lavTech(x, "converged")
                      if (isTRUE(chk) & isTRUE(chk2)) {
                          outi <- fitm0 -
                                    lavaan::fitMeasures(x,
                                          fit.measures = fit_measures,
                                          baseline.model = baseline_model)
                          return(outi)
                        } else {
                          return(rep(NA, length(fitm0)))
                        }
                    }, fitm0 = fitm0)
  if (is.null(dim(out))) {
      out <- matrix(out, length(out), 1)
      colnames(out) <- fit_measures
    } else {
      out <- t(out)
    }
  colnames(out) <- names(fitm0)
  rownames(out) <- case_ids

  attr(out, "call") <- match.call()
  attr(out, "method") <- "leave_one_out"

  class(out) <- c("fit_measures_change", class(out))

  out
}
