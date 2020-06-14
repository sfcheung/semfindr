#'@title
#' Compute the Mahalanobis distance for each case
#'
#'@description
#' Get a [lavaan_rerun()] output and compute the Mahalanobis distance for each case.
#'
#'@details
#' For each case, compute the Mahalanobis distance of each case.
#'
#' Currently only work for one group analysis.
#'
#'@param rerun_out The output from [lavaan_rerun()].
#'
#'@return
#'A one column matrix of the Mahalanobis distance for each case. The row names
#'are the case identification values used in [lavaan_rerun()].
#'
#'@examples
#'# To be prepared.
#'
#'@references
#'Mahalanobis, P. C. (1936). On the generaized distance in statistics. *Proceedings of the National Institute of Science of India, 2*, 49–55.
#'
#'@export

mahalanobis_rerun <- function(rerun_out
                       )
{
  if (missing(rerun_out)) {
      stop("No lavaan_rerun output supplied.")
    }
  case_ids <- names(rerun_out$rerun)
  reruns <- rerun_out$rerun
  dat <- rerun_out$fit@Data@X[[1]]
  md <- stats::mahalanobis(dat, colMeans(dat), stats::cov(dat))
  out <- matrix(md, length(md), 1)
  rownames(out) <- case_ids
  colnames(out) <- "md"
  out
}
