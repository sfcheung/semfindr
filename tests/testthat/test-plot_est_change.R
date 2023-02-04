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

gcd_approx2 <- est_change_approx(fit)

est_to_long <- function(x) {
    tmp <- which(colnames(x) == "gcd_approx")
    x0 <- x[, -tmp]
    out <- data.frame(change = as.vector(x0))
    n <- nrow(x0)
    k <- ncol(x0)
    out$type <- rep(colnames(x0), each = n)
    out$case <- rep(rownames(x0), times = k)
    out$gcd <- rep(x[, "gcd_approx"], times = k)
    out
  }

plot_est_change <- function(x, params) {
    x0 <- est_to_long(x)
    if (!missing(params)) {
        x0 <- x0[x0$type %in% params, ]
      }
    p <- ggplot2::ggplot(data = x0,
                ggplot2::aes(x = .data[["gcd"]],
                             y = .data[["change"]])) +
          ggplot2::geom_point() +
          ggplot2::geom_hline(yintercept = 0)
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[["type"]]),
                        ncol = 1)
    p
  }


params <- c("m1~iv1", "m1~iv2", "dv~m1")
plot_est_change(gcd_approx2, params = params)

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

gcd_approx2 <- est_change_approx(fit, parameters = "=~")
params <- c("f1=~x2", "f2=~x5")
