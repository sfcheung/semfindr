skip("WIP")

library(testthat)
library(lavaan)
library(semfindr)

# A path model
# fixed.x: TRUE (default)
# Labelled: Some are labelled
# User-defined parameters: At least one

mod <-
'
m1 ~ iv1 + a2 * iv2
dv ~ b * m1
a1b := a2 * b
'

dat <- pa_dat

dat0 <- dat[1:50, ]
fit <- lavaan::sem(mod, dat0)

rerun_out <- lavaan_rerun(fit, parallel = FALSE,
                          to_rerun = 1:10)
fit_est_change <- est_change(rerun_out)
fit_est_change_approx <- est_change_approx(fit)
fit_est_change_raw <- est_change_raw(rerun_out)
fit_est_change_raw_approx <- est_change_raw_approx(fit)
fit_est_change_raw_std <- est_change_raw(rerun_out,
                                         standardized = TRUE)

gcd_method <- function(x,
                       gcd_names = c("gcd", "gcd_approx")) {
    gcd_name <- intersect(gcd_names, colnames(x))
    if (length(gcd_name) != 1) {
        stop("The method used cannot be determined")
      }
    gcd_name
  }

est_to_long <- function(x) {
    gcd_name <- gcd_method(x)
    tmp <- which(colnames(x) == gcd_name)
    x0 <- x[, -tmp]
    out <- data.frame(change = as.vector(x0))
    n <- nrow(x0)
    k <- ncol(x0)
    out$param <- rep(colnames(x0), each = n)
    out$case <- rep(rownames(x0), times = k)
    out[, gcd_name] <- rep(x[, gcd_name], times = k)
    attr(out, "gcd_name") <- gcd_name
    out
  }

tmp1 <- est_to_long(fit_est_change)
tmp2 <- est_to_long(fit_est_change_approx)

test_that("Test est_to_long", {
    expect_true("gcd" %in% colnames(tmp1))
    expect_true("gcd_approx" %in% colnames(tmp2))
    expect_equal(nrow(tmp1), nrow(fit_est_change) *
                            (ncol(fit_est_change) - 1))
    expect_equal(nrow(tmp2), nrow(fit_est_change_approx) *
                            (ncol(fit_est_change_approx) - 1))
  })

plot_est_change <- function(x,
                            params) {
    gcd_name <- gcd_method(x)
    approx <- ifelse(gcd_name == "gcd_approx", TRUE, FALSE)
    x0 <- est_to_long(x)
    if (!missing(params)) {
        x0 <- x0[x0$param %in% params, ]
      }
    p <- ggplot2::ggplot(data = x0,
                ggplot2::aes(x = .data[[gcd_name]],
                             y = .data[["change"]])) +
          ggplot2::geom_point() +
          ggplot2::geom_hline(yintercept = 0)
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[["param"]]),
                        ncol = 1,
                        scales = "free_x",
                        strip.position = "left")
    p <- p + ggplot2::xlab(switch(gcd_name,
              gcd = "Generalized Cook's Distance",
              gcd_approx = "Generalized Cook's Distance (Approximated)"))
    p
  }

params <- c("m1~iv1", "a2", "b")
plot_est_change(fit_est_change, params = params)
params <- c("m1~iv1", "a2", "b")
plot_est_change(fit_est_change_raw, params = params)
params <- c("m1~iv1", "m1~iv2", "dv~m1")
plot_est_change(fit_est_change_approx, params = params)

# CFA model with selected loadings

mod <-
'
f1 =~  x1 + x2 + x3
f2 =~  x4 + x5 + x6
f1 ~~ f2
'

dat <- cfa_dat

dat0 <- dat[1:50, ]
fit <- lavaan::cfa(mod, dat0)

fit_est_change_approx <- est_change_approx(fit, parameters = "=~")
params <- c("f1=~x2", "f2=~x5")
