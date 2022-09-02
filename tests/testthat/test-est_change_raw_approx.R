library(testthat)
library(lavaan)
library(semfindr)

#context("Test est_change_raw")

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
# From function
fit_est_change_approx2 <- est_change_raw_approx(fit)

test_that("Checked against known results", {
    expect_equal(ignore_attr = TRUE,
        fit_est_change_approx2,
        fit_est_change_approx
      )
  })

set.seed(1314)
fit_rerun <- lavaan_rerun(fit, to_rerun = 26:50)
fit_est_change <- est_change_raw(fit_rerun)

test_that("Exact and approximate results are similar", {
    abs_diff <- abs(fit_est_change_approx2[26:50, ] -
                    fit_est_change)
    expect_lt(max(abs_diff), expected = .01)
  })
