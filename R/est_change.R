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
#'
#' Currently only work for one group analysis.
#'
#'@param rerun_out The output from [lavaan_rerun()].
#'@param parameters A vector of characters to specify the selected parameters.
#'                  Each element is of this form: `x ~ y` or `x ~~ y`, 
#'                  corresponds to how each parameter is specified in `lavaan`.
#'                  The naming convention of a `lavaan` output can be found
#'                  by [lavaan::parameterEstimates()]. If `NULL`, the default,
#'                  differences on all parameters will be computed.
#'
#'@return
#'A matrix with the number of columns equal to the number of requested parameters,
#'and the number of rows equal to the number of cases. The row names is the 
#'case identification values used in [lavaan_rerun()]. The elements are the 
#'standardized difference. Please see Pek and MacCallum (2011), Equation 7.
#'
#'@examples
#'# To be prepared.
#'
#'@references
#'Pek, J., & MacCallum, R. (2011). Sensitivity analysis in structural equation models: Cases and their influence. *Multivariate Behavioral Research, 46*(2), 202–228. <https://doi.org/10.1080/00273171.2011.561068>
#'
#'@export
#'@importMethodsFrom lavaan vcov

est_change <- function(rerun_out,
                       parameters = NULL
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
                tmpfct,
                est = est0,
                parameters_names = parameters_names,
                parameters_selected = parameters_selected
              )
  out <- t(out)
  out
}
