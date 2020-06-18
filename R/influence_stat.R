#'@title
#' Compute case influence on parameter estimates and fit measures and extremeness
#'
#'@description
#' Get a [lavaan_rerun()] output and compute the changes in selected parameters
#' and fit measures for each case.
#'
#'@details
#' For each case, compute the differences in the estimates of selected parameters
#' and fit measures 
#' between rerun without
#' this case and the original fit with this case. Can also request measures of 
#' extremeness (only Mahalanobis distance is available for now).
#'
#' A wrapper of [est_change()] and [fit_measures_change()].
#'
#' Currently only work for one group analysis.
#'
#'@param rerun_out The output from [lavaan_rerun()].
#'@param fit_measures The argument `fit.measures` used in 
#'                    [lavaan::fitMeasures]. Default
#'                    is `c("chisq", "cfi", "tli")`. If `FALSE`, changes 
#'                    in fit measures will not be computed.
#'@param baseline_model The argument `baseline.model` 	used in      
#'                      [lavaan::fitMeasures]. Default
#                       is `NULL`.
#'@param parameters A vector of characters to specify the selected parameters.
#'                  Each element is of this form: `x ~ y` or `x ~~ y`, 
#'                  corresponds to how each parameter is specified in `lavaan`.
#'                  The naming convention of a `lavaan` output can be found
#'                  by [lavaan::parameterEstimates()]. If `NULL`, the default,
#'                  differences on all parameters will be computed.
#'                  If `FALSE`, changes in parameter estimates will not be computed.
#'@param mahalanobis If `TRUE`, will call [mahalanobis_rerun()] to compute the
#'                   Mahalanobis distance. Default is `TRUE`.
#'@param keep_fit If `TRUE`, will keep the original `lavaan` output will full 
#'                sample as an attribute to the output. For other functions to 
#'                extract necessary information. Default is `TRUE`.
#'
#'@return
#'A matrix with the number of columns equal to the number of requested statistics,
#'and the number of rows equal to the number of cases. The row names is the 
#'case identification values used in [lavaan_rerun()]. Please refer to the corresponding
#'functions for further details.
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
#'# Get all default influence stats
#'out <- influence_stat(fit_rerun)
#'head(out)
#'
#'@references
#'Pek, J., & MacCallum, R. (2011). Sensitivity analysis in structural equation models: Cases and their influence. *Multivariate Behavioral Research, 46*(2), 202–228. <https://doi.org/10.1080/00273171.2011.561068>
#'
#'@seealso [fit_measures_change()], [est_change()], and [mahalanobis_rerun()].
#'@export


influence_stat <- function(
                       rerun_out,
                       fit_measures = c("chisq", "cfi", "tli"),
                       baseline_model = NULL,
                       parameters = NULL,
                       mahalanobis = TRUE,
                       keep_fit = TRUE
                       )
{
  if (missing(rerun_out)) {
      stop("No lavaan_rerun output supplied.")
    }
  case_ids <- names(rerun_out$rerun)
  if (!isFALSE(fit_measures)) {
      fm <- fit_measures_change(rerun_out,
                                fit_measures = fit_measures,
                                baseline_model = baseline_model
                                )
      fm_names <- rownames(fm)
    } else {
      fm <- NULL
      fm_names <- NULL
    }
  if (!isFALSE(parameters)) {
      es <- est_change(rerun_out,
                                parameters = parameters
                                )
      es_names <- rownames(es)
    } else {
      es <- NULL
      es_names <- NULL
    }
  if (isTRUE(mahalanobis)) {
      mh <- mahalanobis_rerun(rerun_out)
      mh_names <- rownames(mh)
    } else {
      mh <- NULL
      mh_names <- NULL
    }
  
  if (!all(is.null(fm), is.null(es), is.null(mh))) {
      tmp_names <- list(fm_names, es_names, mh_names)
      tmp_names <- tmp_names[!sapply(tmp_names, is.null)]
      if (length(tmp_names) > 1) {
          names_check <- sapply(tmp_names[-1], identical, tmp_names[[1]])
          if (!all(names_check)) {
              stop("The row names of the output are not all identical. Something is wrong.")
            }
        }
      out <- cbind(fm, es, mh)
    } else {
      stop("No statistics are requested. Something is wrong.")
    }
  if (keep_fit) {
      attr(out, "fit") <- rerun_out$fit
    }
  out
}
