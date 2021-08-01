#' @title
#' Compute the Mahalanobis distance for each case
#'
#' @description
#' Get a [lavaan_rerun()] output and compute the Mahalanobis distance for each
#'  case.
#'
#' @details
#' For each case, compute the Mahalanobis distance of each case.
#'
#' Currently only work for one group analysis.
#'
#' @param rerun_out The output from [lavaan_rerun()].
#'
#' @return
#' A one-column matrix (a column vector) of the Mahalanobis distance for each
#'  case. The row names
#' are the case identification values used in [lavaan_rerun()].
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
#' # Compute the Mahalanobis distance for each case
#' out <- mahalanobis_rerun(fit_rerun)
#' # Results excluding a case, for the first few cases
#' head(out)
#' # Compute the Mahalanobis distance using stats::mahalanobis
#' md1 <- stats::mahalanobis(dat, colMeans(dat), stats::cov(dat))
#' # Compare the results
#' head(md1)
#'
#' @references
#' Mahalanobis, P. C. (1936). On the generaized distance in statistics.
#'  *Proceedings of the National Institute of Science of India, 2*, 49–55.
#'
#' @export

mahalanobis_rerun <- function(rerun_out
                       ) {
  if (missing(rerun_out)) {
      stop("No lavaan_rerun output supplied.")
    }
  case_ids <- names(rerun_out$rerun)
  dat <- rerun_out$fit@Data@X[[1]]
  md <- stats::mahalanobis(dat, colMeans(dat), stats::cov(dat))
  out <- matrix(md, length(md), 1)
  rownames(out) <- case_ids
  colnames(out) <- "md"
  # No need to check the dimenson. The result is always a column vector
  out
}
