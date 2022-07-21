library(testthat)
library(lavaan)
library(semfindr)

#context("Test est_change")

mod <-
'
iv1 ~~ iv2
m1 ~ iv1 + iv2
dv ~ m1
'

dat <- pa_dat

dat0 <- dat[1:50, ]
fit <- lavaan::sem(mod, dat0)

# From scores
fit_est_change_approx <- lavScores(fit) %*% vcov(fit) *
  nobs(fit) / (nobs(fit) - 1)
# Hessian (inverse of covariance) with scale adjustment
information_fit <- lavInspect(fit, what = "information") * (nobs(fit) - 1)
# Compare information_fit with vcov
tmp1 <- solve(lavTech(fit, what = "information") * (nobs(fit)))
tmp2 <- lavTech(fit, "vcov")
# Short cut for computing quadratic form (https://stackoverflow.com/questions/27157127/efficient-way-of-calculating-quadratic-forms-avoid-for-loops)
gcd_approx <- rowSums(
  (fit_est_change_approx %*% information_fit) * fit_est_change_approx
)

gcd_approx2 <- est_change_approx(fit)

test_that("Check against known results", {
    expect_equal(ignore_attr = TRUE,
        gcd_approx2[, "gcd_approx"],
        gcd_approx
      )
  })
