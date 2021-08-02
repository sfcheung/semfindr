#' @title
#' Compute the Mahalanobis distance for each case using only the exogenous
#' observed variables
#'
#' @description
#' Get a [lavaan_rerun()] output and compute the Mahalanobis distance for each
#'  case using only the exogenous observed variables
#'
#' @details
#' For each case, compute the Mahalanobis distance of each case.
#'
#' Currently only work for one group analysis.
#'
#' @param fit It can be the output from `lavaan`, such as [lavaan::cfa()] and
#'        [lavaan::sem()], or the output from  [lavaan_rerun()].
#' @param emNorm_arg A list of argument for [norm2::emNorm()]. Default is
#'                   `list(estimate.worst = FALSE, criterion = 1e-6)`. Ignored
#'                   if there is no missing data on the exogenous observed 
#'                   variables.
#'
#' @return
#' A one-column matrix (a column vector) of the Mahalanobis distance for each
#'  case. The number of rows equal to the data stored in the fit object.
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
#' 
#' md_exo <- mahalanobis_exo(fit)
#' md_exo
#'
#' @references
#' Mahalanobis, P. C. (1936). On the generaized distance in statistics.
#'  *Proceedings of the National Institute of Science of India, 2*, 49–55.
#'
#' @export

mahalanobis_exo <- function(fit,
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
      fit_data <- lavaan::lavInspect(fit, "data")
      colnames(fit_data) <- lavaan::lavNames(fit)
      fit_free <- lavaan::lavInspect(fit, "free")
    }
  if (inherits(fit, "lavaan_rerun")) {
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
      md_exo <- modi::MDmiss(fit_data_exo,
                            em_out$param$beta,
                            em_out$param$sigma)
    } else {
      md_exo <- stats::mahalanobis(fit_data_exo,
                            colMeans(fit_data_exo),
                            stats::cov(fit_data_exo))
    }

  out <- matrix(md_exo, length(md_exo), 1)
  colnames(out) <- "md"
  # No need to check the dimenson. The result is always a column vector
  out
}
