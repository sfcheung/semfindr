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
#' distance for each case. The row names are the case identification
#' values used in [lavaan_rerun()].
#' A print method is available for user-friendly output.
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
      if (lavaan::lavInspect(fit, "nlevels") > 1) {
          stop("Currently does not support models with more than one level.")
        }
    }

  if (inherits(fit, "lavaan")) {
      fit_data <- lav_data_used(fit)
      ngroups <- lavaan::lavInspect(fit, "ngroups")
      if (ngroups > 1) {
          gp_var <- lavaan::lavInspect(fit, "group")
        } else {
          gp_var <- NULL
        }
    }

  if (inherits(fit, "lavaan_rerun")) {
      fit_data <- lav_data_used(fit$fit)
      ngroups <- lavaan::lavInspect(fit$fit, "ngroups")
      if (ngroups > 1) {
          gp_var <- lavaan::lavInspect(fit$fit, "group")
        } else {
          gp_var <- NULL
        }
    }

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
    }
  if (inherits(fit, "lavaan_rerun")) {
      case_ids <- names(fit$rerun)
    }

  if ((sum(stats::complete.cases(fit_data))) != nrow(fit_data)) {
      missing_data <- TRUE
      if (!requireNamespace("modi", quietly = TRUE)) {
          stop(paste("Missing data is present but the modi package",
                    "is not installed."))
        }
   } else {
      missing_data <- FALSE
   }

  md <- md_i(fit_data = fit_data,
             ngroups = ngroups,
             gp_var = gp_var,
             emNorm_arg = emNorm_arg)

  if (inherits(fit, "lavaan_rerun")) {
      md <- md[fit$selected]
    }
  out <- matrix(md, length(md), 1)
  rownames(out) <- case_ids
  colnames(out) <- "md"
  # No need to check the dimension. The result is always a column vector

  attr(out, "call") <- match.call()
  attr(out, "missing_data") <- missing_data
  # attr(out, "em_out") <- em_out

  class(out) <- c("md_semfindr", class(out))

  out
}

#' @noRd

md_i <- function(fit_data,
                 ngroups,
                 gp_var,
                 emNorm_arg = list(estimate.worst = FALSE,
                                   criterion = 1e-6)) {

    if (ngroups == 1) {
        out <- md_i_single(fit_data = fit_data,
                           emNorm_arg = emNorm_arg)
      } else {
        out <- md_i_multi(fit_data = fit_data,
                          ngroups = ngroups,
                          gp_var = gp_var,
                          emNorm_arg = emNorm_arg)
      }
    return(out)
  }

#' @noRd

md_i_single <- function(fit_data,
                        emNorm_arg = list(estimate.worst = FALSE,
                                          criterion = 1e-6)) {
    # out_na <- matrix(NA, nrow(fit_data), 1)
    #colnames(out_na) <- "md"
    out_na <- rep(NA, nrow(fit_data))

    if ((sum(stats::complete.cases(fit_data))) != nrow(fit_data)) {
        missing_data <- TRUE
        if (!requireNamespace("modi", quietly = TRUE)) {
            stop(paste("Missing data is present but the modi package",
                      "is not installed."))
          }
        suppressWarnings(em_out_fit <- lavaan::lavCor(fit_data,
                                                      missing = "fiml",
                                                      output = "fit"))
        em_out <- list(param = list())
        tmp <- lavaan::lavInspect(em_out_fit, "implied")
        em_out$param$beta <- tmp$mean
        em_out$param$sigma <- tmp$cov
        md <- modi::MDmiss(fit_data,
                              em_out$param$beta,
                              em_out$param$sigma)
      } else {
        missing_data <- FALSE
        em_out <- NULL
        md <- stats::mahalanobis(fit_data,
                              colMeans(fit_data),
                              stats::cov(fit_data))
      }
    return(md)
  }

#' @noRd

md_i_multi <- function(fit_data,
                       ngroups,
                       gp_var,
                       emNorm_arg = list(estimate.worst = FALSE,
                                         criterion = 1e-6)) {
    if (ngroups == 1) {
        gp_var <- make.unique(c(paste0(sample(letters, 5), collapse = ""),
                                colnames(fit_data)))[1]
        fit_data[, gp_var] <- 1
      }
    gp_labels <- unique(fit_data[, gp_var])
    id <- seq_len(nrow(fit_data))
    rownames(fit_data) <- id
    j <- which(colnames(fit_data) == gp_var)
    data_gp <- split(fit_data[, -j, drop = FALSE], fit_data[, gp_var])
    out_1 <- lapply(data_gp,
                    md_i_single,
                    emNorm_arg = emNorm_arg)
    names(out_1) <- NULL
    out <- unlist(out_1)
    out <- out[order(as.numeric(names(out)))]
    return(out)
  }
