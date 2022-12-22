#' @title Case Influence Measures
#'
#' @description Gets a [lavaan_rerun()] output and computes the changes
#'  in selected parameters and fit measures for each case.
#'
#' @details For each case, [influence_stat()] computes the differences
#'  in the estimates of selected parameters and fit measures with and
#'  without this case. Users can request measures of extremeness (only
#'  Mahalanobis distance is available for now).
#'
#' If `rerun_out` is the output of [lavaan_rerun()], it will use the
#' leave-one-out approach.
#' Measures are computed by [est_change()] and [fit_measures_change()].
#'
#' If `rerun_out` is the output of [lavaan::lavaan()] or its wrappers
#' (e.g., [lavaan::cfa()] or [lavaan::sem()]), it will use the
#' approximate approach.
#' Measures are computed by [est_change_approx()] and
#' [fit_measures_change_approx()].
#'
#' Currently it only works for single-group models.
#'
#' @param rerun_out The output from [lavaan_rerun()], or the output
#'  of [lavaan::lavaan()] or its wrappers (e.g., [lavaan::cfa()]
#'  and [lavaan::sem()]).
#' @param fit_measures The argument `fit.measures` used in
#'  [lavaan::fitMeasures]. Default is
#'  `c("chisq", "cfi", "rmsea", "tli")`.
#' @param baseline_model The argument `baseline.model` used in
#'  [lavaan::fitMeasures]. Default is `NULL`.
#' @param parameters A character vector to specify the selected
#'  parameters. Each parameter is named as in `lavaan` syntax, e.g.,
#'  `x ~ y` or `x ~~ y`, as appeared in the columns `lhs`, `op`, and `rhs`
#'  in the output of [lavaan::parameterEstimates()]. If `NULL`, the
#'  default, differences on all free parameters will be computed.
#' @param mahalanobis If `TRUE`, it will call [mahalanobis_rerun()] to
#'  compute the Mahalanobis distance. Default is `TRUE`.
#' @param keep_fit If `TRUE`, it will keep the original `lavaan` output
#'  using the full sample as an attribute to the output. It can be used
#'  by other functions to extract necessary information. Default is
#'  `TRUE`.
#'
#' @return A matrix with the number of columns equals to the number of
#'  requested statistics, and the number of rows equals to the number of
#'  cases. The row names are the case identification values used in
#'  [lavaan_rerun()]. Please refer to the help pages of [est_change()] and
#'  [fit_measures_change()] for details.
#'
#' @author Shu Fai Cheung (shufai.cheung@gmail.com)
#'
#' @examples
#' library(lavaan)
#' dat <- pa_dat
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
#' # Fit the model n times. Each time with one case removed.
#' # For illustration, do this only for selected cases
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
#'                           to_rerun = 1:10)
#' # Get all default influence stats
#' out <- influence_stat(fit_rerun)
#' head(out)
#'
#' @references
#' Pek, J., & MacCallum, R. (2011). Sensitivity analysis in structural equation
#'  models: Cases and their influence. *Multivariate Behavioral Research,
#'  46*(2), 202–228. doi:10.1080/00273171.2011.561068
#'
#' @seealso [fit_measures_change()], [est_change()], and [mahalanobis_rerun()].
#'
#' @export


influence_stat <- function(
                       rerun_out,
                       fit_measures = c("chisq", "cfi", "rmsea", "tli"),
                       baseline_model = NULL,
                       parameters = NULL,
                       mahalanobis = TRUE,
                       keep_fit = TRUE
                       ) {
  if (missing(rerun_out)) {
      stop("No output supplied.")
    }
  if (!(inherits(rerun_out, "lavaan_rerun") ||inherits(rerun_out, "lavaan") )) {
      stop("rerun_out is neither the output of lavaan_rerun or lavaan.")
    }
  if (inherits(rerun_out, "lavaan_rerun")) {
      rerun_out_type <- "lavaan_rerun"
    }
  if (inherits(rerun_out, "lavaan")) {
      rerun_out_type <- "lavaan"
    }
  if (!isFALSE(fit_measures)) {
      fm <- switch(rerun_out_type,
                   lavaan_rerun = fit_measures_change(rerun_out,
                                    fit_measures = fit_measures,
                                    baseline_model = baseline_model),
                   lavaan =  fit_measures_change_approx(rerun_out,
                                    fit_measures = fit_measures,
                                    baseline_model = baseline_model))
      fm_names <- rownames(fm)
    } else {
      fm <- NULL
      fm_names <- NULL
    }
  if (!isFALSE(parameters)) {
      es <- switch(rerun_out_type,
                   lavaan_rerun = est_change(rerun_out,
                                    parameters = parameters),
                   lavaan =  est_change_approx(rerun_out,
                                    parameters = parameters))
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
              stop(paste("The row names of the output are not all identical.",
                         "Something is wrong."))
            }
        }
      out <- cbind(fm, es, mh)
    } else {
      stop("No statistics are requested. Something is wrong.")
    }
  if (keep_fit) {
      attr(out, "fit") <- switch(rerun_out_type,
                                 lavaan_rerun = rerun_out$fit,
                                 lavaan = rerun_out)
    }
  out
}
