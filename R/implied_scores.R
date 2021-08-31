#' @title
#' Implied scores for observed outcome variables
#'
#' @description
#' Get a [lavaan::lavaan()] output and compute the implied scores.
#'
#' @details
#' The implied scores for each observed outcome variable are simply computed
#' in the same way the predicted scores in a linear regression model are
#' computed.
#'
#' It currently supports only single-sample path analytic models with only
#' observed variables.
#'
#' @param fit The output from [lavaan::lavaan()], such as [lavaan::cfa()] and
#'        [lavaan::sem()].
#'
#' @return
#' A matrix of the implied scores.
#'
#' @author S. F. Cheung (shufai.cheung@gmail.com)
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
#' # Compute the implied scores for `m1` and `dv`
#' fit_implied_scores <- implied_scores(fit)
#' head(fit_implied_scores)
#'
#'
#' @importFrom rlang .data
#' @importFrom lavaan fitted
#' @export implied_scores

implied_scores <- function(fit) {

    if (missing(fit)) {
        stop("lavaan output is missing.")
      }

    if (!inherits(fit, "lavaan")) {
        stop("The fit object is not a lavaan output.")
      }

    # if (nrow(lavaan::lavInspect(fit, "pattern")) != 1) {
    #     stop("Datasets with missing data is not yet supported.")
    #   }

    if (lavaan::lavInspect(fit, "ngroups") != 1) {
        stop(paste0("The model has more than one group. \n",
                    "Multiple group analysis not yet supported."))
      }

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

    check_data <- tryCatch(lavaan::lavInspect(fit, "data"),
                            error = function(e) e)
    if (inherits(check_data, "simpleError")) {
        stop("Raw data not available. Implied scores cannot be computed.")
      }

    # dat <- as.data.frame(lavaan::lavInspect(fit, "data"))
    dat <- as.data.frame(fit@Data@X[[1]])
    colnames(dat) <- fit@Data@ov.names[[1]]
    n <- nrow(dat)

    v_names <- fit@Data@ov.names[[1]]
    x_names <- fit@Data@ov.names.x[[1]]
    y_names <- v_names[!(v_names %in% x_names)]
    # The following three lines do not work
    # v_names <- lavaan::lavNames(fit, "ov")
    # x_names <- lavaan::lavNames(fit, "eqs.x")
    # y_names <- lavaan::lavNames(fit, "eqs.y")

    dat_y <- dat[, y_names]
    dat_x <- dat[, x_names]
    if (!lavaan::lavInspect(fit, "meanstructure")) {
        dat_x <- scale(dat_x, center = TRUE, scale = FALSE)
      }

    beta_l  <- lavaan::inspect(fit, "est")$beta
    psi_l   <- lavaan::inspect(fit, "est")$psi
    alpha_l <- lavaan::inspect(fit, "est")$alpha

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
    implied_cov <- fitted(fit)$cov
    class(implied_cov) <- "matrix"
    implied_cov_yy <- implied_cov[y_names, y_names]
    check_implied_cov_yy <- all(round(implied_cov_yy, 5) == round(sigma_yy, 5))
    implied_cov_xx <- implied_cov[x_names, x_names]
    check_implied_cov_xx <- all(round(implied_cov_xx, 5) == round(sigma_xx, 5))
    implied_cov_xy <- implied_cov[x_names, y_names]
    check_implied_cov_xy <- all(round(implied_cov_xy, 5) == round(sigma_xy, 5))

    implied_mean <- fitted(fit)$mean
    class(implied_mean) <- "numeric"
    implied_mean_y <- implied_mean[y_names]
    check_implied_mean_y <- all(round(mean_y, 5) == round(implied_mean_y, 5))

    y_hat <-  matrix(alpha, n, p, byrow = TRUE) +
                as.matrix(dat_y) %*% t(beta) + as.matrix(dat_x) %*% t(gamma)

    fit_rsquare_raw <- lavaan::parameterEstimates(fit, rsquare = TRUE)
    fit_rsquare_raw <- fit_rsquare_raw[fit_rsquare_raw$op == "r2", ]
    fit_rsquare <- fit_rsquare_raw$est
    names(fit_rsquare) <- fit_rsquare_raw$lhs
    dat_y <- dat[, y_names]
    y_hat_rsquare <- rep(NA, p)
    names(y_hat_rsquare) <- y_names
    for (i in y_names) {
        y_hat_rsquare[i] <-
            suppressWarnings(stats::cor(dat[, i], y_hat[, i],
                             use = "pairwise.complete.obs")^2)
    }
    y_hat_rsquare <- y_hat_rsquare[!is.na(y_hat_rsquare)]
    fit_rsquare
    check_rsquare <- all(round(y_hat_rsquare, 5) == round(fit_rsquare, 5))

    check_summary <- vector(mode = "character")
    if (!check_implied_cov_yy) {
        check_summary <- c(check_summary,
            "Implied covariances matrix of Y variables cannot be reproduced.")
    }
    if (!check_implied_cov_xx) {
        check_summary <- c(check_summary,
            "Implied covariances matrix of X variables cannot be reproduced.")
    }
    if (!check_implied_cov_xy) {
        check_summary <- c(check_summary,
        "Implied covariances matrix of X vs. Y variables cannot be reproduced.")
    }
    if (lavaan::lavInspect(fit, "meanstructure")) {
        if (!check_implied_mean_y) {
            check_summary <- c(check_summary,
                "Implied means of Y variables cannot be reproduced.")
        }
      } 
    if (!check_rsquare) {
        check_summary <- c(check_summary,
            "Implied R-sauares of Y variables cannot be reproduced.")
    }

    if (length(check_summary) > 0) {
        stop(paste0(paste("Some results cannot be reproduced.",
                          "The implied scores cannot be trusted. \n"),
                    paste0(check_summary, collapse = "\n")))
    }

    no_square_names <- names(fit_rsquare)

    y_hat[, colnames(y_hat) %in% no_square_names]

  }
