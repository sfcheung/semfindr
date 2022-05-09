#' @title Case Influence on Fit Measures
#'
#' @description Gets a [lavaan_rerun()] output and computes the change
#'  in selected fit measures if a case is deleted
#'
#' @details For each case, [fit_measures_change()] computes the
#'  differences in selected fit measures with and without this case.
#'
#' If the analysis is not admissible or does not converge when a case
#'  is deleted, `NA`s will be turned for the differences of this
#'  case.
#'
#' Currently it only supports single-group models.
#'
#' @param rerun_out The output from [lavaan_rerun()].
#' @param fit_measures The argument `fit.measures` used in
#'  [lavaan::fitMeasures]. Default is 
#'  `c("chisq", "cfi", "rmsea", "tli")`.
#' @param baseline_model The argument `baseline.model` used in
#'  [lavaan::fitMeasures]. Default is `NULL`.
#'
#' @return A matrix with the number of columns equals to the number of
#'  requested fit measures, and the number of rows equals to the number
#'  of cases. The row names are the case identification values used in
#'  [lavaan_rerun()].
#'
#' @author Shu Fai Cheung (shufai.cheung@gmail.com)
#'
#' @references Pek, J., & MacCallum, R. (2011). Sensitivity analysis
#'  in structural equation models: Cases and their influence.
#'  *Multivariate Behavioral Research, 46*(2), 202–228.
#'  doi:10.1080/00273171.2011.561068
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
#' # Fit the model n times. Each time with one case removed.
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE)
#' # Compute the changes in chisq if a case is removed
#' out <- fit_measures_change(fit_rerun, fit_measures = "chisq")
#' # Results excluding a case, for the first few cases
#' head(out)
#' # Chi-square will all cases
#' (chisq_all <- fitMeasures(fit, c("chisq")))
#' # Chi-square with the first case removed
#' fit_01 <- lavaan::sem(mod, dat[-1, ])
#' (chisq_no_1 <- fitMeasures(fit_01, c("chisq")))
#' # Difference
#' chisq_all - chisq_no_1
#' # Compare to the result from the fit_measures_change
#' out[1, ]
#' @export

fit_measures_change <- function(rerun_out,
                         fit_measures = c("chisq", "cfi", "rmsea", "tli"),
                         baseline_model = NULL
                         ) {
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
  out
}
