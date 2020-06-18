#'@title
#' Case influence on parameter estimates
#'
#'@description
#' Get a [lavaan_rerun()] output and compute the changes in selected parameters
#' for each case. 
#'
#'@details
#' For each case, compute the differences in the estimates of selected parameter
#' between rerun without
#' this case and the original fit with this case.
#' The change is the raw change, either for the standardized or
#' unstandardized solution. The change is *not* divided by standard error.#'
#' Currently only work for one group analysis.
#'
#'@param rerun_out The output from [lavaan_rerun()].
#'@param parameters A vector of characters to specify the selected parameters.
#'                  Each element is of this form: `x ~ y` or `x ~~ y`, 
#'                  corresponds to how each parameter is specified in `lavaan`.
#'                  The naming convention of a `lavaan` output can be found
#'                  by [lavaan::parameterEstimates()]. If `NULL`, the default,
#'                  differences on all parameters will be computed.
#'@param standardized If `TRUE`, the changes in full standardized is returned.
#'                    Otherwise, the changes in unstandardized solution is returned.
#'                    Default is `FALSE`.
#'
#'@return
#'A matrix with the number of columns equal to the number of requested parameters,
#'and the number of rows equal to the number of cases. The row names is the 
#'case identification values used in [lavaan_rerun()]. The elements are the 
#'standardized difference. Please see Pek and MacCallum (2011), Equation 7.
#'
#'@examples
#'library(lavaan)
#'dat <- pa_dat
#'# For illustration only, select only the first 50 cases
#'dat <- dat[1:50, ]
#'# The model
#'mod <- 
#''
#'m1 ~ iv1 + iv2
#'dv ~ m1
#''
#'# Fit the model
#'fit <- lavaan::sem(mod, dat)
#'summary(fit)
#'# Fit the model n times. Each time with one case removed.
#'fit_rerun <- lavaan_rerun(fit, parallel = FALSE)
#'# Compute the changes in parameter estimates if a case is removed
#'out <- est_change_raw(fit_rerun)
#'# Results excluding a case, for the first few cases
#'head(out)
#'# Note that these are the differences parameter estimates.
#'
#'# The parameter estimates from all cases
#'(coef_all <- coef(fit))
#'# The parameter estimates from manually deleting the first case
#'fit_no_1 <- lavaan::sem(mod, dat[-1, ])
#'(coef_no_1 <- coef(fit_no_1))
#'# The differences
#'coef_all - coef_no_1
#'
#'# Compute the changes for the paths from iv1 and iv2 to m1
#'out2 <- est_change_raw(fit_rerun, c("m1 ~ iv1", "m1 ~ iv2"))
#'# Results excluding a case, for the first few cases
#'head(out2)
#'# Note that only the changes in the selected paths are included.
#'
#'# Use standardized = TRUE to compare the differences in standardized solution
#'out2_std <- est_change_raw(fit_rerun, c("m1 ~ iv1", "m1 ~ iv2"), standardized = TRUE)
#'head(out2_std)
#'parameterEstimates(fit, standardized = TRUE)[1:2, c("lhs", "op", "rhs", "std.all")]
#'parameterEstimates(fit_no_1, standardized = TRUE)[1:2, c("lhs", "op", "rhs", "std.all")]
#'out2_std[1, ]
#'
#'@references
#'Pek, J., & MacCallum, R. (2011). Sensitivity analysis in structural equation models: Cases and their influence. *Multivariate Behavioral Research, 46*(2), 202–228. <https://doi.org/10.1080/00273171.2011.561068>
#'
#'@export
#'@importMethodsFrom lavaan vcov

est_change_raw <- function(rerun_out,
                       parameters = NULL,
                       standardized = FALSE
                       )
{
  if (missing(rerun_out)) {
      stop("No lavaan_rerun output supplied.")
    }
  case_ids <- names(rerun_out$rerun)
  reruns <- rerun_out$rerun
  fit0   <- rerun_out$fit
  est0   <- lavaan::parameterEstimates(
              fit0,
              se = FALSE,
              zstat = FALSE,
              pvalue = FALSE,
              ci = FALSE,
              standardized = TRUE,
              fmi = FALSE,
              cov.std = TRUE,
              rsquare = FALSE,
              remove.nonfree = TRUE,
              output = "data.frame"
              )
  parameters_names <- paste0(est0$lhs, est0$op, est0$rhs)
  if (!is.null(parameters)) {
    parameters_selected <- gsub(" ", "", parameters)
    if (!all(parameters_selected %in% parameters_names)) {
        stop("Not all parameters can be found in the output. Please check the parameters argument.")
      }
    } else {
      parameters_selected <- parameters_names
    }
  tmpfct <- function(x, est, parameters_names,
                             parameters_selected) {
    esti_full <- lavaan::parameterEstimates(
                  x,
                  se = TRUE,
                  zstat = FALSE,
                  pvalue = FALSE,
                  ci = FALSE,
                  standardized = TRUE,
                  fmi = FALSE,
                  cov.std = TRUE,
                  rsquare = FALSE,
                  remove.nonfree = TRUE,
                  output = "data.frame"
                  )
    if (standardized) {
        esti_change <- (est$std.all - esti_full$std.all)
      } else {
        esti_change <- (est$est - esti_full$est)
      }
    names(esti_change) <- parameters_names
    outi <- esti_change[parameters_selected]
    names(outi) <- parameters_selected
    outi
  }
  out <- sapply(reruns,
                tmpfct,
                est = est0,
                parameters_names = parameters_names,
                parameters_selected = parameters_selected
              )
  if (is.null(dim(out))) {
      out <- matrix(out, length(out), 1)
      colnames(out) <- parameters
    } else {
      out <- t(out)
    }
  rownames(out) <- case_ids
  out
}
