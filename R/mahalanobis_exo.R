#' @title Mahalanobis Distance (Observed Predictors)
#'
#' @description Gets a [lavaan_rerun()] or [lavaan::lavaan()] output
#'  and computes the Mahalanobis distance for each case using only the
#'  observed predictors.
#'
#' @details For each case, [mahalanobis_predictors()] computes the
#'  Mahalanobis distance of each case on the observed predictors.
#'
#' If there are no missing values, [stats::mahalanobis()] will be used
#'  to compute the Mahalanobis distance.
#'
#' If there are missing values on the observed predictors, the means
#'  and variance-covariance matrices will be estimated by maximum
#'  likelihood using [norm2::emNorm()]. The estimates will be passed
#'  to [modi::MDmiss()] to compute the Mahalanobis distance.
#'
#' Currently this function only supports single-group models.
#'
#' @param fit It can be the output from `lavaan`, such as
#'  [lavaan::cfa()] and [lavaan::sem()], or the output from
#'  [lavaan_rerun()].
#' @param emNorm_arg A list of argument for [norm2::emNorm()].
#'  Default is `list(estimate.worst = FALSE, criterion = 1e-6)`.
#'  Ignored if there is no missing data on the observed
#'  predictors.
#'
#' @return A one-column matrix (a column vector) of the Mahalanobis
#'  distance for each case. The number of rows equals to the number of
#'  cases in the data stored in the fit object.
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
#'
#' md_predictors <- mahalanobis_predictors(fit)
#' md_predictors
#'
#' @author Shu Fai Cheung (shufai.cheung@gmail.com)
#'
#' @references
#'
#' Béguin, C., & Hulliger, B. (2004). Multivariate outlier detection in
#' incomplete survey data: The epidemic algorithm and transformed rank
#' correlations. *Journal of the Royal Statistical Society: Series A
#' (Statistics in Society)*, 167(2), 275–294.
#'
#' Mahalanobis, P. C. (1936). On the generalized distance in statistics.
#' *Proceedings of the National Institute of Science of India, 2*, 49–55.
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
  if (!inherits(fit, "lavaan") & !inherits(fit, "lavaan_rerun")) {
      stop("The fit object must of of the class 'lavaan' or 'lavaan_rerun'.")
    }
  if (inherits(fit, "lavaan")) {
      if (lavaan::lavInspect(fit, "ngroups") > 1) {
          stop("Currently only support single group models.")
        }
      if (lavaan::lavInspect(fit, "nclusters") > 1) {
          stop("Currently does not support models with more than one cluster.")
        }
      if (lavaan::lavInspect(fit, "nlevels") > 1) {
          stop("Currently does not support models with more than one level.")
        }
    }
  if (inherits(fit, "lavaan")) {
      case_ids <- lavaan::lavInspect(fit, "case.idx")
      fit_data <- lavaan::lavInspect(fit, "data")
      colnames(fit_data) <- lavaan::lavNames(fit)
      fit_free <- lavaan::lavInspect(fit, "free")
    }
  if (inherits(fit, "lavaan_rerun")) {
      case_ids <- names(fit$rerun)
      fit_data <- lavaan::lavInspect(fit$fit, "data")
      colnames(fit_data) <- lavaan::lavNames(fit$fit)
      fit_free <- lavaan::lavInspect(fit$fit, "free")
    }

  out_na <- matrix(NA, nrow(fit_data), 1)
  colnames(out_na) <- "md"

  if (is.null(fit_free$beta)) {
      warning("The model has no exogenous observed variables.")
      return(out_na)
    }

  i <- apply(fit_free$beta, 1, function(x) all(x == 0))
  exo_vars <- names(i)[i]
  exo_vars <- exo_vars[exo_vars %in% colnames(fit_data)]
  if (length(exo_vars) == 0) {
      warning("The model has no exogenous observed variables.")
      return(out_na)
    }
  fit_data_exo <- fit_data[, exo_vars, drop = FALSE]
  if ((sum(stats::complete.cases(fit_data_exo))) != nrow(fit_data_exo)) {
      if (!requireNamespace("modi", quietly = TRUE)) {
          stop(paste("Missing data is present but the modi package",
                     "is not installed."))
        }
      if (!requireNamespace("norm2", quietly = TRUE)) {
          stop(paste("Missing data is present but the norm2 package",
                     "is not installed."))
        }
      emNorm_arg_final <- utils::modifyList(list(),
                                    emNorm_arg)
      em_out <- tryCatch(do.call(norm2::emNorm,
                                 c(list(obj = fit_data_exo),
                                 emNorm_arg_final)),
                         error = function(e) e)
      if (inherits(em_out, "SimpleError")) {
          warning("Missing data is present but norm2::emNorm raised an error.")
          warning(em_out)
          return(out_na)
        }
      if (!em_out$converged) {
          warning("Missing data is present but norm2::emNorm did not converge.")
          return(out_na)
        }
      md_predictors <- modi::MDmiss(fit_data_exo,
                            em_out$param$beta,
                            em_out$param$sigma)
    } else {
      md_predictors <- stats::mahalanobis(fit_data_exo,
                            colMeans(fit_data_exo),
                            stats::cov(fit_data_exo))
    }
  if (inherits(fit, "lavaan_rerun")) {
      md_predictors <- md_predictors[fit$selected]
    }
  out <- matrix(md_predictors, length(md_predictors), 1)
  rownames(out) <- case_ids
  colnames(out) <- "md"
  # No need to check the dimension. The result is always a column vector
  out
}
