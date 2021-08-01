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
#' @param fit The output from `lavaan`, such as [lavaan::cfa()] and
#'        [lavaan::sem()].
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

mahalanobis_exo <- function(fit) {
  if (missing(fit)) {
      stop("No lavaan output supplied.")
    }
  if (!inherits(fit, "lavaan")) {
      stop("The fit object must of of the class 'lavaan'.")
    }
  if (lavaan::lavInspect(fit, "ngroups") > 1) {
      stop("Currently only support single group models.")
    }
  if (lavaan::lavInspect(fit, "nclusters") > 1) {
      stop("Currently does not support models with more than one cluster.")
    }
  if (lavaan::lavInspect(fit, "nlevels") > 1) {
      stop("Currently does not support models with more than one level.")
    }

  fit_data <- lavaan::lavInspect(fit, "data")
  colnames(fit_data) <- lavaan::lavNames(fit)

  fit_free <- lavaan::lavInspect(fit, "free")
  i <- apply(fit_free$beta, 1, function(x) all(x == 0))
  exo_vars <- names(i)[i]
  fit_data_exo <- fit_data[, exo_vars]
  if ((length(complete.cases(fit_data_exo))) != nrow(fit_data_exo)) {
      stop(paste("Currently does not support missing data on the exogenous",
                 "variables."))
    }
  md_exo <- stats::mahalanobis(fit0_data_exo,
                        colMeans(fit0_data_exo),
                        cov(fit0_data_exo))

  out <- matrix(md_exo, length(md_exo), 1)
  colnames(out) <- "md"
  # No need to check the dimenson. The result is always a column vector
  out
}
