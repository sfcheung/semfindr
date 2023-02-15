#' @title Mahalanobis Distance on All Observed Variables
#'
#' @description Computes the Mahalanobis distance for each case on all
#' observed variables in a model.
#'
#' @details [mahalanobis_rerun()] gets a [lavaan_rerun()] or
#' [lavaan::lavaan()] output and computes the Mahalanobis distance for
#' each case on all observed variables.
#'
#' If there are no missing values, [stats::mahalanobis()] will be used
#' to compute the Mahalanobis distance.
#'
#' If there are missing values on the observed predictors, the means
#' and variance-covariance matrices will be estimated by maximum
#' likelihood using [norm2::emNorm()]. The estimates will be passed
#' to [modi::MDmiss()] to compute the Mahalanobis distance.
#'
#' Currently it only supports single-group models.
#'
#' @param fit It can be the output from `lavaan`, such as
#' [lavaan::cfa()] and [lavaan::sem()], or the output from
#' [lavaan_rerun()].
#'
#' @param emNorm_arg A list of argument for
#' [norm2::emNorm()]. Default is
#' `list(estimate.worst = FALSE, criterion = 1e-6)`.
#' Ignored if there is no missing data on the exogenous observed
#' variables.
#'
#' @return A one-column matrix (a column vector) of the Mahalanobis
#' distance for each case. The row names are the case identification
#' values used in [lavaan_rerun()].
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.
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
#' # For illustration, do this only for selected cases.
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
#'                           to_rerun = 1:10)
#' # Compute the Mahalanobis distance for each case
#' out <- mahalanobis_rerun(fit_rerun)
#' # Results excluding a case, for the first few cases
#' head(out)
#' # Compute the Mahalanobis distance using stats::mahalanobis()
#' md1 <- stats::mahalanobis(dat, colMeans(dat), stats::cov(dat))
#' # Compare the results
#' head(md1)
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
#' mahalanobis_rerun(fit_rerun)
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
#' fit <- lavaan::cfa(mod, dat)
#'
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
#'                           to_rerun = 1:10)
#' mahalanobis_rerun(fit_rerun)
#'
#'
#' @references Mahalanobis, P. C. (1936). On the generalized distance
#'  in statistics. *Proceedings of the National Institute of Science
#'  of India, 2*, 49-55.
#'
#' @export

mahalanobis_rerun <- function(fit,
                              emNorm_arg = list(estimate.worst = FALSE,
                                                criterion = 1e-6))
{
  if (missing(fit)) {
      stop("No fit object supplied.")
    }
  if (!inherits(fit, "lavaan") && !inherits(fit, "lavaan_rerun")) {
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
      fit_data <- lavaan::lavInspect(fit, "data")
      colnames(fit_data) <- lavaan::lavNames(fit)
      fit_free <- lavaan::lavInspect(fit, "free")
    }
  if (inherits(fit, "lavaan_rerun")) {
      fit_data <- lavaan::lavInspect(fit$fit, "data")
      colnames(fit_data) <- lavaan::lavNames(fit$fit)
      fit_free <- lavaan::lavInspect(fit$fit, "free")
    }

  if (missing(fit)) {
      stop("No fit object supplied.")
    }
  if (!inherits(fit, "lavaan") && !inherits(fit, "lavaan_rerun")) {
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
    }
  if (inherits(fit, "lavaan_rerun")) {
      case_ids <- names(fit$rerun)
      fit_data <- lavaan::lavInspect(fit$fit, "data")
    }

  out_na <- matrix(NA, nrow(fit_data), 1)
  colnames(out_na) <- "md"

  if ((sum(stats::complete.cases(fit_data))) != nrow(fit_data)) {
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
                                 c(list(obj = fit_data),
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
      md <- modi::MDmiss(fit_data,
                            em_out$param$beta,
                            em_out$param$sigma)
    } else {
      md <- stats::mahalanobis(fit_data,
                            colMeans(fit_data),
                            stats::cov(fit_data))
    }
  if (inherits(fit, "lavaan_rerun")) {
      md <- md[fit$selected]
    }
  out <- matrix(md, length(md), 1)
  rownames(out) <- case_ids
  colnames(out) <- "md"
  # No need to check the dimension. The result is always a column vector
  out
}
