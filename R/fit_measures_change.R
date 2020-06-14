#'@title
#' Case influence on fit measures from `lavaan` fitMeasures
#'
#'@description
#' Get a [lavaan_rerun()] output and compute the change in selected fit measures 
#' for each case.
#'
#'@details
#' For each case, compute the difference in selected fit measures between rerun without
#' this case and the original fit with this case.
#'
#' Currently only work for one group analysis.
#'
#'@param rerun_out The output from [lavaan_rerun()].
#'@param fit_measures The argument `fit.measures` used in 
#'                    [lavaan::fitMeasures]. Default
#'                    is `c("chisq", "cfi", "tli")`.
#'@param baseline_model The argument `baseline.model` 	used in      
#'                      [lavaan::fitMeasures]. Default
#                       is `NULL`.
#'
#'@return
#'A matrix with the number of columns equal to the number of requested fit measures,
#'and the number of rows equal to the number of cases. The row names is the 
#'case identification values used in [lavaan_rerun()].
#'
#'@references
#'Pek, J., & MacCallum, R. (2011). Sensitivity analysis in structural equation models: Cases and their influence. *Multivariate Behavioral Research, 46*(2), 202–228. <https://doi.org/10.1080/00273171.2011.561068>
#'
#'@examples
#'# To be prepared.
#'
#'@export

fit_measures_change <- function(rerun_out,
                         fit_measures = c("chisq", "cfi", "tli"),
                         baseline_model = NULL
                         )
{
  if (missing(rerun_out)) {
      stop("No lavaan_rerun output supplied.")
    }
  case_ids <- names(rerun_out)
  reruns <- rerun_out$rerun
  fit0   <- rerun_out$fit
  fitm0  <- lavaan::fitMeasures(fit0, fit.measures = fit_measures,
                                      baseline.model = baseline_model)
  out <- sapply(reruns, function(x, fitm0) {
                      fitm0 -
                      lavaan::fitMeasures(x, fit.measures = fit_measures,
                                             baseline.model = baseline_model)
                    }, fitm0 = fitm0)
  out <- t(out)
}
