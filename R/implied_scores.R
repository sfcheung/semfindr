#' @title Implied Scores of Observed Outcome Variables
#'
#' @description Gets a [lavaan::lavaan()] output and computes the
#' implied scores of observed outcome variables.
#'
#' @details The implied scores for each observed outcome variable
#' (the `y`-variables or the endogenous variables) are
#' simply computed in the same way the predicted scores in a linear
#' regression model are computed.
#'
#' Currently it supports only single-group and multiple-group
#' path analysis models with only
#' observed variables.
#' (Support for multiple-group models available in 0.1.4.8 and later version).
#'
#' @param fit The output from [lavaan::lavaan()], such as [lavaan::cfa()]
#' and [lavaan::sem()].
#'
#' @param output Output type. If `"matrix"`, the default,
#' the output will be combined to one matrix, with cases ordered as
#' in the original dataset (after listwise deletion, if used). If
#' `"list"`, the a list of matrices is returned, even if the model
#' has only one group.
#'
#' @param skip_all_checks If `TRUE`, skips all checks and allows
#' users to run this function on any object of `lavaan` class.
#' For users to experiment this and other functions on models
#' not officially supported. Default is `FALSE`.
#'
#' @return A matrix of the implied scores if `output` is `"matrix"`.
#' If `output` is `"list"`, a list of matrices of the implied scores.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.
#'
#' @examples
#' library(lavaan)
#' dat <- pa_dat
#' # For illustration, select only the first 50 cases
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
#' # Compute the implied scores for `m1` and `dv`
#' fit_implied_scores <- implied_scores(fit)
#' head(fit_implied_scores)
#'
#'
#' @importFrom rlang .data
#' @importFrom lavaan fitted
#' @export implied_scores

implied_scores <- function(fit,
                           output = "matrix",
                           skip_all_checks = FALSE) {

    if (missing(fit)) {
        stop("lavaan output is missing.")
      }

    if (!inherits(fit, "lavaan")) {
        stop("The fit object is not a lavaan output.")
      }

    # if (nrow(lavaan::lavInspect(fit, "pattern")) != 1) {
    #     stop("Datasets with missing data is not yet supported.")
    #   }

    if (!skip_all_checks) {
        # if (lavaan::lavInspect(fit, "ngroups") != 1) {
        #     stop(paste0("The model has more than one group. \n",
        #                 "Multiple group analysis not yet supported."))
        #   }

        # if (is.null(lavaan::inspect(fit, "est")$alpha)) {
        #     stop(paste0("Mean structure not analyzed. It is required."))
        #   }

        if (lavaan::lavInspect(fit, "nlevels") != 1) {
            stop(paste0("The model has more than one level. \n",
                        "Multiple group analysis not yet supported."))
          }

        if (max(unlist(lavaan::lavInspect(fit, "nclusters"))) > 1) {
            stop(paste0("Clustered mode not yet supported."))
          }
      }

    check_data <- tryCatch(lavaan::lavInspect(fit, "data"),
                            error = function(e) e)
    if (inherits(check_data, "simpleError")) {
        stop("Raw data not available. Implied scores cannot be computed.")
      }

    dat_full <- lav_data_used(fit)
    ngroups <- lavaan::lavInspect(fit, "ngroups")
    group_label <- lavaan::lavInspect(fit, "group.label")
    dat <- lavaan::lavInspect(fit, "data",
                              drop.list.single.group = FALSE)
    case_idx <- lavaan::lavInspect(fit, "case.idx",
                                   drop.list.single.group = FALSE)
    case_idx_full <- unlist(case_idx, use.names = FALSE)
    fit_implied <- lavaan::lavInspect(fit, "fitted",
                                      drop.list.single.group = FALSE)
    implied_cov <- lapply(fit_implied, function(x) x$cov)
    implied_mean <- lapply(fit_implied, function(x) x$mean)
    mm <- lavaan::lavInspect(fit, "est",
                             drop.list.single.group = FALSE)
    fit_rsquare <- implied_rsquare(fit)

    out0 <- mapply(implied_scores_i,
                   dat = dat,
                   mm = mm,
                   implied_cov = implied_cov,
                   implied_mean = implied_mean,
                   fit_rsquare = fit_rsquare,
                   MoreArgs = list(fit = fit),
                   SIMPLIFY = FALSE)
    if (identical(output, "matrix")) {
        out <- do.call(rbind, out0)
        out <- out[order(case_idx_full), , drop = FALSE]
        rownames(out) <- case_idx_full[order(case_idx_full)]
      } else {
        out <- mapply(function(x, y) {rownames(x) <- y; x},
                      x = out0,
                      y = case_idx,
                      SIMPLIFY = FALSE)
      }
    return(out)
  }

#' @noRd
# check_rsquare disabled by default due to the computation
# of r-squares in multiple-group models.
# It is sufficient to check implied cov and means.

implied_scores_i <- function(fit,
                             dat,
                             mm,
                             implied_cov,
                             implied_mean,
                             fit_rsquare,
                             check_rsquare = FALSE) {
    n <- nrow(dat)
    v_names <- lavaan::lavNames(fit, "ov")
    y_names <- lavaan::lavNames(fit, "eqs.y")
    x_names <- v_names[!(v_names %in% y_names)]

    dat_y <- dat[, y_names]
    dat_x <- dat[, x_names]
    if (!lavaan::lavInspect(fit, "meanstructure")) {
        dat_x <- scale(dat_x, center = TRUE, scale = FALSE)
      }

    beta_l  <- mm$beta
    psi_l   <- mm$psi
    alpha_l <- mm$alpha

    beta    <- beta_l[y_names, y_names]
    gamma   <- beta_l[y_names, x_names]
    psi     <- psi_l[y_names, y_names]
    phi     <- psi_l[x_names, x_names]
    if (lavaan::lavInspect(fit, "meanstructure")) {
        alpha <- as.matrix(alpha_l[y_names, ])
        x_means <- alpha_l[x_names, ]
      } else {
        alpha <- matrix(0, length(y_names), 1)
        rownames(alpha) <- y_names
        x_means <- matrix(0, length(x_names), 1)
        rownames(x_means) <- x_names
      }

    p <- length(y_names)
    i <- diag(p)
    sigma_yy <- solve(i - beta) %*% (gamma %*% phi %*% t(gamma) + psi) %*%
                solve(i - t(beta))
    sigma_xx <- phi
    sigma_xy <- phi %*% t(gamma) %*% solve(i - t(beta))

    mean_y <- solve(i - beta) %*% alpha + solve(i - beta) %*% gamma %*% x_means

    # Check computation
    class(implied_cov) <- "matrix"
    implied_cov_yy <- implied_cov[y_names, y_names]
    check_implied_cov_yy <- all(round(implied_cov_yy, 5) == round(sigma_yy, 5))
    implied_cov_xx <- implied_cov[x_names, x_names]
    check_implied_cov_xx <- all(round(implied_cov_xx, 5) == round(sigma_xx, 5))
    implied_cov_xy <- implied_cov[x_names, y_names]
    check_implied_cov_xy <- all(round(implied_cov_xy, 5) == round(sigma_xy, 5))

    class(implied_mean) <- "numeric"
    implied_mean_y <- implied_mean[y_names]
    check_implied_mean_y <- all(round(mean_y, 5) == round(implied_mean_y, 5))

    y_hat <-  matrix(alpha, n, p, byrow = TRUE) +
                as.matrix(dat_y) %*% t(beta) + as.matrix(dat_x) %*% t(gamma)

    dat_y <- dat[, y_names]
    y_hat_rsquare <- rep(NA, p)
    names(y_hat_rsquare) <- y_names
    for (i in y_names) {
        y_hat_rsquare[i] <-
            suppressWarnings(stats::cor(dat[, i], y_hat[, i],
                             use = "pairwise.complete.obs")^2)
    }
    y_hat_rsquare <- y_hat_rsquare[!is.na(y_hat_rsquare)]
    check_rsquare <- all(round(y_hat_rsquare, 5) == round(fit_rsquare, 5))

    check_summary <- vector(mode = "character")
    if (!check_implied_cov_yy) {
        check_summary <- c(check_summary,
            "Implied covariances matrix of y variables cannot be reproduced.")
    }
    if (!check_implied_cov_xx) {
        check_summary <- c(check_summary,
            "Implied covariances matrix of x variables cannot be reproduced.")
    }
    if (!check_implied_cov_xy) {
        check_summary <- c(check_summary,
        "Implied covariances matrix of x vs. y variables cannot be reproduced.")
    }
    if (lavaan::lavInspect(fit, "meanstructure")) {
        if (!check_implied_mean_y) {
            check_summary <- c(check_summary,
                "Implied means of y-variables cannot be reproduced.")
        }
      }
    if (!check_rsquare && check_rsquare) {
        check_summary <- c(check_summary,
            "Implied R-squares of y-variables cannot be reproduced.")
    }

    if (length(check_summary) > 0) {
        stop(paste0(paste("Some results cannot be reproduced.",
                          "The implied scores cannot be trusted. \n"),
                    paste0(check_summary, collapse = "\n")))
    }

    no_square_names <- names(fit_rsquare)

    out <- y_hat[, colnames(y_hat) %in% no_square_names]

    return(out)
  }

#' @noRd

implied_rsquare <- function(fit) {
    fit_rsquare_raw <- lavaan::parameterEstimates(fit, rsquare = TRUE)
    ngroups <- lavaan::lavInspect(fit, "ngroups")
    if (ngroups == 1) {
        fit_rsquare_raw$group <- 1
      }
    fit_rsquare_raw <- fit_rsquare_raw[fit_rsquare_raw$op == "r2", ]
    fit_rsquare <- fit_rsquare_raw$est
    names(fit_rsquare) <- fit_rsquare_raw$lhs
    out <- split(fit_rsquare, fit_rsquare_raw$group)
    if (ngroups > 1) {
        names(out) <- lavaan::lavInspect(fit, "group.label")
      }
    return(out)
  }
