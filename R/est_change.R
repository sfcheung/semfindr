#' @title
#' Case influence on parameter estimates by standardized change
#'
#' @description
#' Get a [lavaan_rerun()] output and compute the standardized changes in
#'  selected parameters for each case.
#'
#' @details
#' For each case, compute the standardized differences in the estimates of
#' selected parameter
#' between rerun without
#' this case and the original fit with this case.
#' The differences are standardized by dividing the raw differences by their
#' standard errors.
#'
#' If the analysis is not admissible or did not converge when a case was
#' deleted, `NA` will be turned for this case.
#'
#' Currently only work for one group analysis.
#'
#' @param rerun_out The output from [lavaan_rerun()].
#' @param parameters A vector of characters to specify the selected parameters.
#'                   Each element is of this form: `x ~ y` or `x ~~ y`,
#'                   corresponds to how each parameter is specified in `lavaan`.
#'                   The naming convention of a `lavaan` output can be found
#'                   by [lavaan::parameterEstimates()]. If `NULL`, the default,
#'                   standardized differences on all parameters will be
#'                   computed.
#'
#' @return
#' A matrix with the number of columns equal to the number of requested
#' parameters,
#' and the number of rows equal to the number of cases. The row names is the
#' case identification values used in [lavaan_rerun()]. The elements are the
#' standardized difference. Please see Pek and MacCallum (2011), Equation 7.
#'
#' @references
#' Pek, J., & MacCallum, R. (2011). Sensitivity analysis in structural equation
#'  models: Cases and their influence. *Multivariate Behavioral Research,
#'  46*(2), 202–228. <https://doi.org/10.1080/00273171.2011.561068>
#'
#' @examples
#' library(lavaan)
#' dat <- pa_dat
#' # For illustration only, select only the first 50 cases
#' dat <- dat[1:50, ]
#' # The model
#' mod <-
#' '
#' m1 ~ iv1 + iv2
#' dv ~ m1
#' '
#' # Fit the model
#' fit <- lavaan::sem(mod, dat)
#' summary(fit)
#' # Fit the model n times. Each time with one case removed.
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE)
#' # Compute the changes in chisq if a case is removed
#' out <- est_change(fit_rerun)
#' # Results excluding a case, for the first few cases
#' head(out)
#' # Note that these are the differences divided by the standard error
#' # The right most column contains the generalized Cook's distances.
#'
#' # Compute the changes for the paths from iv1 and iv2 to m1
#' out2 <- est_change(fit_rerun, c("m1 ~ iv1", "m1 ~ iv2"))
#' # Results excluding a case, for the first few cases
#' head(out2)
#' # Note that only the changes in the selected paths are included.
#' # The generalized Cook's distance is computed only from the selected
#' # parameter estimates.
#'
#' @references
#' Pek, J., & MacCallum, R. (2011). Sensitivity analysis in structural equation
#'  models: Cases and their influence. *Multivariate Behavioral Research,
#'  46*(2), 202–228. <https://doi.org/10.1080/00273171.2011.561068>
#'
#' @export
#' @importMethodsFrom lavaan vcov

est_change <- function(rerun_out,
                       parameters = NULL
                       ) {
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
              standardized = FALSE,
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
        stop(paste("Not all parameters can be found in the output.",
                   "Please check the parameters argument."))
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
                  standardized = FALSE,
                  fmi = FALSE,
                  cov.std = TRUE,
                  rsquare = FALSE,
                  remove.nonfree = TRUE,
                  output = "data.frame"
                  )
    esti_change <- (est$est - esti_full$est)/esti_full$se
    names(esti_change) <- parameters_names
    esti_change <- esti_change[parameters_selected]
    vcovi_full <- vcov(x)
    class(vcovi_full) <- "matrix"
    vcovi_full <- vcovi_full[parameters_selected, parameters_selected]
    k <- length(esti_change)
    esti_change_raw <- (est$est - esti_full$est)
    names(esti_change_raw) <- parameters_names
    esti_change_raw <- esti_change_raw[parameters_selected]
    gcdi <- matrix(esti_change_raw, 1, k) %*% solve(vcovi_full) %*% 
            matrix(esti_change_raw, k, 1)
    outi <- c(esti_change, gcdi)
    names(outi) <- c(parameters_selected, "gcd")
    outi
  }
  out <- sapply(reruns,
                function(x, est, parameters_names, parameters_selected) {
                  chk <- suppressWarnings(lavaan::lavTech(x, "post.check"))
                  if (isTRUE(chk)) {
                      return(tmpfct(x, est = est,
                                    parameters_names = parameters_names,
                                    parameters_selected = parameters_selected)
                            )
                    } else {
                      return(rep(NA, length(parameters_selected) + 1))
                    }
                  },
                est = est0,
                parameters_names = parameters_names,
                parameters_selected = parameters_selected
              )
  # No need to check the number of columns because it is always greater than one

  out <- t(out)
  colnames(out) <- c(parameters_selected, "gcd")
  rownames(out) <- case_ids
  out
}
