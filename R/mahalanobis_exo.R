#' @title Mahalanobis Distance On Observed Predictors
#'
#' @description Gets a [lavaan_rerun()] or [lavaan::lavaan()] output
#' and computes the Mahalanobis distance for each case using only the
#' observed predictors.
#'
#' @details For each case, [mahalanobis_predictors()] computes the
#' Mahalanobis distance of each case on the observed predictors.
#'
#' If there are no missing values, [stats::mahalanobis()] will be used
#' to compute the Mahalanobis distance.
#'
#' If there are missing values on the observed predictors, the means
#' and variance-covariance matrices will be estimated by maximum
#' likelihood using [lavaan::lavCor()]. The estimates will be passed
#' to [modi::MDmiss()] to compute the Mahalanobis distance.
#'
#' Supports both single-group and multiple-group models.
#' For multiple-group models, the Mahalanobis distance for
#' each case is computed using the means and covariance matrix
#' of the group this case belongs to.
#' (Support for multiple-group models available in 0.1.4.8 and later version).
#'
#' @param fit It can be the output from `lavaan`, such as
#' [lavaan::cfa()] and [lavaan::sem()], or the output from
#' [lavaan_rerun()].
#'
#' @param emNorm_arg No longer used. Kept for backward
#' compatibility.
#'
#' @return A `md_semfindr`-class object, which is
#' a one-column matrix (a column vector) of the Mahalanobis
#' distance for each case. The number of rows equals to the number of
#' cases in the data stored in the fit object.
#' A print method is available for user-friendly output.
#'
#' @examples
#' library(lavaan)
#' dat <- pa_dat
#' # For illustration, select only the first 50 cases.
#' dat <- dat[1:50, ]
#' # The model
#' mod <-
#' "
#' m1 ~ a1 * iv1 +  a2 * iv2
#' dv ~ b * m1
#' a1b := a1 * b
#' a2b := a2 * b
#' "
#' # Fit the model
#' fit <- lavaan::sem(mod, dat)
#' summary(fit)
#'
#' md_predictors <- mahalanobis_predictors(fit)
#' md_predictors
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.
#'
#' @references
#'
#' Béguin, C., & Hulliger, B. (2004). Multivariate outlier detection in
#' incomplete survey data: The epidemic algorithm and transformed rank
#' correlations. *Journal of the Royal Statistical Society: Series A
#' (Statistics in Society)*, 167(2), 275-294.
#'
#' Mahalanobis, P. C. (1936). On the generalized distance in statistics.
#' *Proceedings of the National Institute of Science of India, 2*, 49-55.
#'
#' Schafer, J.L. (1997) *Analysis of incomplete multivariate data*.
#' Chapman & Hall/CRC Press.
#'
#' @export

mahalanobis_predictors <- function(fit,
                                   emNorm_arg = list(estimate.worst = FALSE,
                                                     criterion = 1e-6)) {
  if (missing(fit)) {
      stop("No fit object supplied.")
    }
  if (!inherits(fit, "lavaan") && !inherits(fit, "lavaan_rerun")) {
      stop("The fit object must of of the class 'lavaan' or 'lavaan_rerun'.")
    }
  if (inherits(fit, "lavaan")) {
      if (lavaan::lavInspect(fit, "nlevels") > 1) {
          stop("Currently does not support models with more than one level.")
        }
    }
  if (inherits(fit, "lavaan")) {
      case_ids <- sort(unlist(lavaan::lavInspect(fit, "case.idx"),
                       use.names = FALSE))
      fit_data <- lav_data_used(fit)
      ngroups <- lavaan::lavInspect(fit, "ngroups")
      if (ngroups > 1) {
          gp_var <- lavaan::lavInspect(fit, "group")
        } else {
          gp_var <- NULL
        }
      exo_vars <- setdiff(lavaan::lavNames(fit, "eqs.x"),
                          lavaan::lavNames(fit, "eqs.y"))
      exo_vars <- intersect(lavaan::lavNames(fit, "ov"), exo_vars)
    }
  if (inherits(fit, "lavaan_rerun")) {
      case_ids <- names(fit$rerun)
      fit_data <- lav_data_used(fit$fit)
      ngroups <- lavaan::lavInspect(fit$fit, "ngroups")
      if (ngroups > 1) {
          gp_var <- lavaan::lavInspect(fit$fit, "group")
        } else {
          gp_var <- NULL
        }
      exo_vars <- setdiff(lavaan::lavNames(fit$fit, "eqs.x"),
                          lavaan::lavNames(fit$fit, "eqs.y"))
      exo_vars <- intersect(lavaan::lavNames(fit$fit, "ov"), exo_vars)
    }

  md_predictors <- matrix(NA, length(case_ids), 1)
  colnames(md_predictors) <- "md"
  rownames(md_predictors) <- case_ids

  missing_data <- NA

  if (length(exo_vars) == 0) {
      warning("The model has no exogenous observed variables.")
    } else {
      fit_data_exo <- fit_data[, c(exo_vars, gp_var), drop = FALSE]
      if ((sum(stats::complete.cases(fit_data_exo))) != nrow(fit_data_exo)) {
            missing_data <- TRUE
            if (!requireNamespace("modi", quietly = TRUE)) {
                stop(paste("Missing data is present but the modi package",
                          "is not installed."))
              }
        } else {
            missing_data <- FALSE
        }
      md_predictors <- md_i(fit_data = fit_data_exo,
                            ngroups = ngroups,
                            gp_var = gp_var,
                            emNorm_arg = emNorm_arg)
    }

  if (inherits(fit, "lavaan_rerun")) {
      md_predictors <- md_predictors[fit$selected]
    }
  out <- matrix(md_predictors, length(md_predictors), 1)
  rownames(out) <- case_ids
  colnames(out) <- "md"
  # No need to check the dimension. The result is always a column vector

  attr(out, "call") <- match.call()
  attr(out, "missing_data") <- missing_data
  # attr(out, "em_out") <- em_out
  attr(out, "exo_vars") <- exo_vars

  class(out) <- c("md_semfindr", class(out))

  out
}
